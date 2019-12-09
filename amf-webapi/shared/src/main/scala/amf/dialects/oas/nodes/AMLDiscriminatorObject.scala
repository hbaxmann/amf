package amf.dialects.oas.nodes
import amf.core.metamodel.domain.ShapeModel
import amf.plugins.document.vocabularies.model.domain.PropertyMapping
import amf.dialects.OAS30Dialect.DialectLocation
import amf.core.vocabulary.Namespace.XsdTypes.xsdString
import amf.plugins.domain.shapes.metamodel.NodeShapeModel

object AMLDiscriminatorObject extends DialectNode {
  override def name: String = "DiscriminatorObject"

  override def nodeTypeMapping: String = "DiscriminatorObject.id"

  override def properties: Seq[PropertyMapping] = Seq(
    PropertyMapping()
      .withId(DialectLocation + s"#/declarations/DiscriminatorObject/propertyName")
      .withNodePropertyMapping(NodeShapeModel.Discriminator.value.iri())
      .withName("propertyName")
      .withLiteralRange(xsdString.iri()),
    PropertyMapping()
      .withId(DialectLocation + s"#/declarations/DiscriminatorObject/mapping")
      .withName("mapping")
      .withLiteralRange(xsdString.iri())
  )
}
