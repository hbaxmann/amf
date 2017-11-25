package amf.plugins.document.webapi.parser.spec.common

import amf.core.model.domain.DomainElement
import amf.core.parser.{Annotations, _}
import amf.plugins.domain.webapi.models.CustomDomainProperty
import amf.plugins.domain.webapi.models.extensions.DomainExtension
import org.yaml.model._

import scala.collection.mutable.ListBuffer

case class AnnotationParser(lazyElement: () => DomainElement, map: YMap)(implicit val ctx: ParserContext) {
  def parse(): Unit = {
    val domainExtensions: ListBuffer[DomainExtension] = ListBuffer()
    map.entries.foreach { entry =>
      val key = entry.key.as[YScalar].text
      if (WellKnownAnnotation.normalAnnotation(key)) {
        domainExtensions += ExtensionParser(key, lazyElement().id, entry).parse()
      }
    }
    if (domainExtensions.nonEmpty)
      lazyElement().withCustomDomainProperties(domainExtensions)
  }
}

case class ExtensionParser(annotationRamlName: String, parent: String, entry: YMapEntry)(implicit val ctx: ParserContext) {
  def parse(): DomainExtension = {
    val domainExtension = DomainExtension()
    val annotationName  = WellKnownAnnotation.parseName(annotationRamlName)
    val dataNode        = DataNodeParser(entry.value, parent = Some(parent + s"/$annotationName")).parse()
    // TODO
    // throw a parser-side warning validation error if no annotation can be found
    val customDomainProperty = ctx.declarations.annotations.getOrElse(annotationName, CustomDomainProperty(Annotations(entry)).withName(annotationName))
    domainExtension.adopted(parent)
    domainExtension
      .withExtension(dataNode)
      .withDefinedBy(customDomainProperty)
      .withName(annotationName)
  }
}
