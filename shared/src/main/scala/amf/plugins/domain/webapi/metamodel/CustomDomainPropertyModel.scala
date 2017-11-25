package amf.plugins.domain.webapi.metamodel

import amf.core.metamodel.Field
import amf.core.metamodel.Type.{Array, Iri, Str}
import amf.core.metamodel.domain.DomainElementModel
import amf.core.metamodel.domain.templates.KeyField
import amf.plugins.domain.shapes.metamodel.ShapeModel
import amf.plugins.domain.webapi.models.CustomDomainProperty
import amf.core.vocabulary.Namespace.{Document, Rdf, Rdfs, Shapes}
import amf.core.vocabulary.{Namespace, ValueType}

/**
  * Custom Domain Property
  *
  * Definition of an extension to the domain model defined directly by a user in the RAML/OpenAPI document.
  *
  * This can be achieved by using an annotationType in RAML. In OpenAPI thy don't need to
  * be declared, they can just be used.
  *
  * This should be mapped to new RDF properties declared directly in the main document or module.
  *
  * Contrast this extension mechanism with the creation of a propertyTerm in a vocabulary, a more
  * re-usable and generic way of achieving the same functionality
  */
object CustomDomainPropertyModel extends DomainElementModel with KeyField {

  /**
    * The name of the extension
    */
  val Name        = Field(Str, Namespace.Document + "name")
  val DisplayName = Field(Str, Namespace.Schema + "name")
  val Description = Field(Str, Namespace.Schema + "description")

  override val key: Field = Name

  /**
    * These Iris are always going to be domain classes URIs.
    * No any class can be added to the domain.
    *
    * They are mapped from the allowedTargets property of an annotationType in RAML.
    */
  val Domain = Field(Array(Iri), Rdfs + "domain")

  /**
    * A shape constraining the shape of the valid RDF graph for the property.
    * It is parsed from the RAML type associated to the annotationType.
    */
  val Schema = Field(ShapeModel, Shapes + "schema")

  override def fields: List[Field] = List(Domain, Schema, Name) ++ DomainElementModel.fields

  override val `type`: List[ValueType] = Rdf + "Property" :: Document + "DomainProperty" :: DomainElementModel.`type`

  override def modelInstance = CustomDomainProperty()
}
