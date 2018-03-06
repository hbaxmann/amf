package amf.plugins.document.vocabularies2.parser.instances

import amf.core.Root
import amf.core.annotations.Aliases
import amf.core.model.document.{BaseUnit, DeclaresModel}
import amf.core.utils._
import amf.core.parser._
import amf.core.parser.{Annotations, BaseSpecParser, Declarations, EmptyFutureDeclarations, ErrorHandler, FutureDeclarations, ParsedReference, ParserContext, Reference, SearchScope}
import amf.core.vocabulary.Namespace
import amf.plugins.document.vocabularies2.model.document.{Dialect, DialectInstance, DialectInstanceFragment, DialectInstanceLibrary}
import amf.plugins.document.vocabularies2.model.domain._
import amf.plugins.document.vocabularies2.parser.common.SyntaxErrorReporter
import org.yaml.convert.YRead
import org.yaml.model._

import scala.collection.mutable

class DialectInstanceDeclarations(var dialectDomainElements: Map[String, DialectDomainElement] = Map(),
                                  errorHandler: Option[ErrorHandler],
                                  futureDeclarations: FutureDeclarations)
  extends Declarations(Map(), Map(), Map(), errorHandler, futureDeclarations) {

  /** Get or create specified library. */
  override def getOrCreateLibrary(alias: String): DialectInstanceDeclarations = {
    libraries.get(alias) match {
      case Some(lib: DialectInstanceDeclarations) => lib
      case _ =>
        val result = new DialectInstanceDeclarations(errorHandler = errorHandler, futureDeclarations = EmptyFutureDeclarations())
        libraries = libraries + (alias -> result)
        result
    }
  }

  def registerDialectDomainElement(name: String, dialectDomainElement: DialectDomainElement): DialectInstanceDeclarations = {
    dialectDomainElements += (name -> dialectDomainElement)
    if (!dialectDomainElement.isUnresolved) {
      futureDeclarations.resolveRef(name, dialectDomainElement)
    }
    this
  }

  def findDialectDomainElement(key: String, nodeMapping: NodeMapping, scope: SearchScope.Scope): Option[DialectDomainElement] = {
    findForType(key, _.asInstanceOf[DialectInstanceDeclarations].dialectDomainElements, scope) collect {
      case dialectDomainElement: DialectDomainElement if dialectDomainElement.definedBy.id == nodeMapping.id => dialectDomainElement
    }
  }

  override def declarables: Seq[DialectDomainElement] = dialectDomainElements.values.toSeq
}


class DialectInstanceContext(val dialect: Dialect, private val wrapped: ParserContext, private val ds: Option[DialectInstanceDeclarations] = None)
  extends ParserContext(wrapped.rootContextDocument, wrapped.refs, wrapped.futureDeclarations) with SyntaxErrorReporter {

  val declarationsNodeMappings: Map[String, NodeMapping] = parseDeclaredNodeMappings()

  val declarations: DialectInstanceDeclarations =
    ds.getOrElse(new DialectInstanceDeclarations(errorHandler = Some(this), futureDeclarations = futureDeclarations))

  protected def parseDeclaredNodeMappings(): Map[String, NodeMapping] = {
    val declarations = Option(dialect.documents()).flatMap { documents =>
      Option(documents.root()).map { root =>
        root.declaredNodes() map { declaration =>
          findNodeMapping(declaration.mappedNode()) map { nodeMapping =>
            (declaration.name(), nodeMapping)
          }
        } collect { case Some(res: (String, NodeMapping)) => res }
      }
    }.getOrElse(Nil)

    declarations.foldLeft(Map[String, NodeMapping]()) { case (acc, (name, mapping)) =>
      acc + (name -> mapping)
    }
  }

  def findNodeMapping(mappingId: String): Option[NodeMapping] = {
    dialect.declares.collectFirst {
      case mapping: NodeMapping if mapping.id == mappingId => mapping
    }
  }

  private def isInclude(node: YNode) = node.tagType == YType.Include

  def link(node: YNode): Either[String, YNode] = {
    node match {
      case _ if isInclude(node) => Left(node.as[YScalar].text)
      case _                    => Right(node)
    }
  }
}

case class ReferenceDeclarations(references: mutable.Map[String, Any] = mutable.Map())(implicit ctx: DialectInstanceContext) {
  def +=(alias: String, unit: BaseUnit): Unit = {
    references += (alias -> unit)
    unit match {
      case m: DeclaresModel =>
        val library = ctx.declarations.getOrCreateLibrary(alias)
        m.declares.foreach {
          case dialectElement: DialectDomainElement => library.registerDialectDomainElement(dialectElement.localRefName, dialectElement)
          case decl                                 => library += decl
        }
      case f: DialectInstanceFragment =>
        ctx.declarations.fragments += (alias -> f.encodes)
    }
  }

  def baseUnitReferences(): Seq[BaseUnit] =
    references.values.toSet.filter(_.isInstanceOf[BaseUnit]).toSeq.asInstanceOf[Seq[BaseUnit]]
}

case class DialectInstanceReferencesParser(dialectInstance: DialectInstance, map: YMap, references: Seq[ParsedReference])(implicit ctx: DialectInstanceContext) {

  def parse(location: String): ReferenceDeclarations = {
    val result = ReferenceDeclarations()
    parseLibraries(dialectInstance, result, location)
    references.foreach {
      case ParsedReference(f: DialectInstanceFragment, origin: Reference, None) => result += (origin.url, f)
      case _                                                                    =>
    }

    result
  }

  private def target(url: String): Option[BaseUnit] =
    references.find(r => r.origin.url.equals(url)).map(_.unit)


  private def parseLibraries(dialectInstance: DialectInstance, result: ReferenceDeclarations, id: String): Unit = {
    map.key(
      "uses",
      entry =>
        entry.value
          .as[YMap]
          .entries
          .foreach(e => {
            val alias: String = e.key
            val url: String   = library(e)
            target(url).foreach {
              case module: DeclaresModel =>
                collectAlias(dialectInstance, alias -> module.id)
                result += (alias, module)
              case other =>
                ctx.violation(id, s"Expected vocabulary module but found: $other", e) // todo Uses should only reference modules...
            }
          })
    )
  }

  private def library(e: YMapEntry): String = e.value.tagType match {
    case YType.Include => e.value.as[YScalar].text
    case _             => e.value
  }


  private def collectAlias(aliasCollectorUnit: BaseUnit, alias: (String, String)): BaseUnit = {
    aliasCollectorUnit.annotations.find(classOf[Aliases]) match {
      case Some(aliases) =>
        aliasCollectorUnit.annotations.reject(_.isInstanceOf[Aliases])
        aliasCollectorUnit.add(aliases.copy(aliases = aliases.aliases + alias))
      case None => aliasCollectorUnit.add(Aliases(Set(alias)))
    }
  }
}


class RamlDialectInstanceParser(root: Root)(implicit override val ctx: DialectInstanceContext) extends BaseSpecParser {
  val map: YMap = root.parsed.document.as[YMap]


  def parseDocument(): Option[DialectInstance] = {
    val dialectInstance: DialectInstance = DialectInstance(Annotations(map)).withLocation(root.location).withId(root.location + "#").withDefinedBy(ctx.dialect.id)
    parseDeclarations()
    val references = DialectInstanceReferencesParser(dialectInstance, map, root.references).parse(dialectInstance.location)

    val document = parseEncoded(dialectInstance) match {

      case Some(dialectDomainElement) =>
        dialectInstance.withEncodes(dialectDomainElement)
        if (ctx.declarations.declarables.nonEmpty)
          dialectInstance.withDeclares(ctx.declarations.declarables)
        if (references.baseUnitReferences().nonEmpty)
          dialectInstance.withReferences(references.baseUnitReferences())
        Some(dialectInstance)

      case _ => None

    }

    // resolve unresolved references
    ctx.futureDeclarations.resolve()

    document
  }

  def parseFragment(): Option[DialectInstanceFragment] = {
    val dialectInstanceFragment: DialectInstanceFragment = DialectInstanceFragment(Annotations(map)).withLocation(root.location).withId(root.location + "#").withDefinedBy(ctx.dialect.id)
    parseEncodedFragment(dialectInstanceFragment) match {
      case Some(dialectDomainElement) => Some(dialectInstanceFragment.withEncodes(dialectDomainElement))
      case _                          => None
    }
  }

  def parseLibrary(): Option[DialectInstanceLibrary] = throw new Exception("Not implemented yet")

  protected def parseDeclarations(): Unit = {
    ctx.declarationsNodeMappings.foreach { case (name, nodeMapping) =>
        map.entries.find(_.key.as[String] == name).foreach { entry =>
          val declarationsId = root.location + "#/" + name.urlEncoded
          entry.value.as[YMap].entries.foreach { declarationEntry =>
            val id = declarationsId + "/" + declarationEntry.key.as[String].urlEncoded
            parseNode(id, declarationEntry.value, nodeMapping) match {
              case Some(node) => ctx.declarations.registerDialectDomainElement(declarationEntry.key, node)
              case other      => // TODO: violation here
            }
          }
        }
    }
  }

  protected def parseEncoded(dialectInstance: DialectInstance): Option[DialectDomainElement] = {
    Option(ctx.dialect.documents()) flatMap {
      documents: DocumentsModel =>
        Option(documents.root()) flatMap {
          mapping =>
            ctx.findNodeMapping(mapping.encoded()) match {
              case Some(nodeMapping) => parseNode(dialectInstance.id + "/", map, nodeMapping)
              case _ => None
            }
        }
    }
  }

  protected def parseEncodedFragment(dialectInstanceFragment: DialectInstanceFragment): Option[DialectDomainElement] = {
    Option(ctx.dialect.documents()) flatMap {
      documents: DocumentsModel =>
        Option(documents.fragments()).getOrElse(Nil).find { documentMapping =>
          root.parsed.comment.get.metaText.replace(" ", "").contains(documentMapping.documentName())
        } match {
          case Some(documentMapping) =>
            ctx.findNodeMapping(documentMapping.encoded()) match {
              case Some(nodeMapping) => parseNode(dialectInstanceFragment.id + "/", map, nodeMapping)
              case _ => None
            }
          case None => None
        }
    }
  }

  def parseProperty(id: String, propertyEntry: YMapEntry, property: PropertyMapping, node: DialectDomainElement): Unit = {
    property.classification() match {
      case LiteralProperty           => parseLiteralProperty(id, propertyEntry, property, node)
      case LiteralPropertyCollection => parseLiteralCollectionProperty(id, propertyEntry, property, node)
      case ObjectProperty            => parseObjectProperty(id, propertyEntry, property, node)
      case ObjectPropertyCollection  => parseObjectCollectionProperty(id, propertyEntry, property, node)
      case ObjectMapProperty         => parseObjectMapProperty(id, propertyEntry,property, node)
      case _ => // TODO: throw exception
    }
  }

  def checkHashProperties(node: DialectDomainElement, propertyMapping: PropertyMapping, propertyEntry: YMapEntry): DialectDomainElement = {
    // TODO: check if the node already has a value and that it matches (maybe coming from a declaration)
    Option(propertyMapping.mapKeyProperty()) match {
        case Some(propId) => node.setMapKeyField(propId, propertyEntry.key.as[String], propertyEntry.key)
        case None         => node
    }
  }

  def parseObjectProperty(id: String, propertyEntry: YMapEntry, property: PropertyMapping, node: DialectDomainElement): Unit = {
    property.objectRange() match {
      case range: Seq[String] if range.size > 1  => // TODO: parse unions
      case range: Seq[String] if range.size == 1 =>
        ctx.dialect.declares.find(_.id == range.head) match {
          case Some(nodeMapping: NodeMapping) =>
            val nestedObjectId = pathSegment(id, propertyEntry.key.as[String].urlEncoded)
            parseNestedNode(nestedObjectId, propertyEntry.value, nodeMapping) match {
              case Some(dialectDomainElement) => node.setObjectField(property, dialectDomainElement, propertyEntry.value)
              case None                       => // ignore
            }
        }
      case _ => // TODO: throw exception, illegal range
    }
  }

  def parseObjectMapProperty(id: String, propertyEntry: YMapEntry, property: PropertyMapping, node: DialectDomainElement): Unit = {
    val nested = propertyEntry.value.as[YMap].entries.map { keyEntry =>
        property.objectRange() match {
          case range: Seq[String] if range.size > 1  => // TODO: parse unions
          case range: Seq[String] if range.size == 1 =>
            ctx.dialect.declares.find(_.id == range.head) match {
              case Some(nodeMapping: NodeMapping) =>
                val nestedObjectId = pathSegment(id, propertyEntry.key.as[String].urlEncoded) + s"/${keyEntry.key.as[String].urlEncoded}"
                parseNestedNode(nestedObjectId, keyEntry.value, nodeMapping) match {
                  case Some(dialectDomainElement) => Some(checkHashProperties(dialectDomainElement, property, keyEntry))
                  case None                       => None
                }
            }
          case _ => None
        }
    }
    node.setObjectField(property, nested.collect { case Some(node: DialectDomainElement) => node }, propertyEntry.value)
  }

  def parseObjectCollectionProperty(id: String, propertyEntry: YMapEntry, property: PropertyMapping, node: DialectDomainElement): Unit = {
    val res = propertyEntry.value.as[YSequence].nodes.zipWithIndex.map { case (node, nextElem) =>
      property.objectRange() match {
        case range: Seq[String] if range.size > 1  => // TODO: parse unions
        case range: Seq[String] if range.size == 1 =>
          ctx.dialect.declares.find(_.id == range.head) match {
            case Some(nodeMapping: NodeMapping) =>
              val nestedObjectId = pathSegment(id, propertyEntry.key.as[String].urlEncoded) + s"/$nextElem"
              parseNestedNode(nestedObjectId, node, nodeMapping) match {
                case Some(dialectDomainElement) => Some(dialectDomainElement)
                case None                       => None
              }
          }
        case _ => None
      }
    }
    val elems: Seq[DialectDomainElement] = res.collect { case Some(x: DialectDomainElement) => x}
    node.setObjectField(property, elems, propertyEntry.value)
  }

  def pathSegment(parent: String, next: String): String = {
    if (parent.endsWith("/")) {
      parent + next.urlEncoded
    } else {
      parent + "/" + next.urlEncoded
    }
  }

  def parseLiteralValue(value: YNode, property: PropertyMapping, node: DialectDomainElement): Option[_] = {

    implicit val reader:YRead[Float] = new YRead[Float]() {
      override def read(node: YNode): Either[YError, Float] = Right(value.toString.toFloat)
      override def defaultValue: Float = 0
    }

    value.tagType match {
      case YType.Bool if property.literalRange() == (Namespace.Xsd + "boolean").iri() =>
        Some(value.as[Boolean])
      case YType.Bool  =>
        ctx.inconsistentPropertyRangeValueViolation(node.id, property, property.literalRange(), (Namespace.Xsd + "boolean").iri(), value)
        None
      case YType.Int   if property.literalRange() == (Namespace.Xsd + "integer").iri() =>
        Some(value.as[Int])
      case YType.Int  =>
        ctx.inconsistentPropertyRangeValueViolation(node.id, property, property.literalRange(), (Namespace.Xsd + "integer").iri(), value)
        None
      case YType.Str   if property.literalRange() == (Namespace.Xsd + "string").iri() =>
        Some(value.as[String])
      case YType.Str  =>
        ctx.inconsistentPropertyRangeValueViolation(node.id, property, property.literalRange(), (Namespace.Xsd + "string").iri(), value)
        None
      case YType.Float if property.literalRange() == (Namespace.Xsd + "float").iri() =>
        Some(value.as[Float])
      case YType.Float  =>
        ctx.inconsistentPropertyRangeValueViolation(node.id, property, property.literalRange(), (Namespace.Xsd + "float").iri(), value)
        None
      case _           =>
        ctx.violation(node.id, s"Unsupported scalar type ${value.tagType}", value)
        None
    }
  }

  def setLiteralValue(value: YNode, property: PropertyMapping, node: DialectDomainElement) = {
    parseLiteralValue(value, property, node) match {
      case Some(b: Boolean) => node.setLiteralField(property, b, value)
      case Some(i: Int)     => node.setLiteralField(property, i, value)
      case Some(f: Float)   => node.setLiteralField(property, f, value)
      case Some(s: String)  => node.setLiteralField(property, s, value)
      case _                => // ignore
    }
  }

  def parseLiteralProperty(id: String, propertyEntry: YMapEntry, property: PropertyMapping, node: DialectDomainElement): Unit = {
    setLiteralValue(propertyEntry.value, property, node)
  }

  def parseLiteralCollectionProperty(id: String, propertyEntry: YMapEntry, property: PropertyMapping, node: DialectDomainElement): Unit = {
    propertyEntry.value.tagType match {
      case YType.Seq =>
        val values = propertyEntry.value.as[YSequence].nodes.map { elemValue => parseLiteralValue(elemValue, property, node) }.collect { case Some(v) => v }
        node.setLiteralField(property, values, propertyEntry.value)
      case _ =>
        parseLiteralValue(propertyEntry.value, property, node) match {
          case Some(v) => node.setLiteralField(property, Seq(v), propertyEntry.value)
          case _       => // ignore
        }
    }
  }

  protected def parseNestedNode(id: String, entry: YNode, mapping: NodeMapping): Option[DialectDomainElement] =
    parseNode(id, entry, mapping)

  protected def parseNode(id: String, ast: YNode, mapping: NodeMapping): Option[DialectDomainElement] = {
    ast.tagType match {
      case YType.Map =>
        val nodeMap = ast.as[YMap]
        val node: DialectDomainElement = DialectDomainElement(nodeMap).withId(id).withDefinedBy(mapping)
        node.withInstanceTypes(Seq(mapping.nodetypeMapping))
        mapping.propertiesMapping().foreach { propertyMapping =>
          val propertyName = propertyMapping.name()
          nodeMap.entries.find(_.key.as[String] == propertyName) match {
            case Some(entry) => parseProperty(id, entry, propertyMapping, node)
            case None        => // ignore
          }
        }
        Some(node)

      case YType.Str =>
        val refTuple = ctx.link(ast) match {
          case Left(key) =>
            (key, ctx.declarations.findDialectDomainElement(key, mapping, SearchScope.Fragments))
          case _ =>
            val text = ast.as[YScalar].text
            (text, ctx.declarations.findDialectDomainElement(text, mapping, SearchScope.Named))
        }
        refTuple match {
          case (text: String, Some(s)) =>
            val linkedNode = s.link(text, Annotations(ast.value))
              .asInstanceOf[DialectDomainElement]
              .withId(id) // and the ID of the link at that position in the tree, not the ID of the linked element, tha goes in link-target
            Some(linkedNode)
          case (text: String, _) =>
            val linkedNode = DialectDomainElement(map).withId(id)
            linkedNode.unresolved(text, map)
            Some(linkedNode)
        }

      case YType.Include =>
        val refTuple = ctx.link(ast) match {
          case Left(key) =>
            (key, ctx.declarations.findDialectDomainElement(key, mapping, SearchScope.Fragments))
          case _ =>
            val text = ast.as[YScalar].text
            (text, ctx.declarations.findDialectDomainElement(text, mapping, SearchScope.Named))
        }
        refTuple match {
          case (text: String, Some(s)) =>
            val linkedNode = s.link(text, Annotations(ast.value))
              .asInstanceOf[DialectDomainElement]
              .withId(id) // and the ID of the link at that position in the tree, not the ID of the linked element, tha goes in link-target
            Some(linkedNode)
          case (text: String, _) =>
            val linkedNode = DialectDomainElement(map).withId(id)
            linkedNode.unresolved(text, map)
            Some(linkedNode)
        }

      case _         => None // TODO violation here
    }
  }


}
