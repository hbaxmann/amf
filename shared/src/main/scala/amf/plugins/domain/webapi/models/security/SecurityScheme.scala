package amf.plugins.domain.webapi.models.security

import amf.core.model.domain
import amf.core.model.domain.{AmfArray, DomainElement, Linkable}
import amf.core.parser.{Annotations, Fields}
import amf.plugins.domain.webapi.metamodel.security.SecuritySchemeModel.{Settings => SettingsField}
import amf.plugins.domain.shapes.models.Shape
import amf.plugins.domain.webapi.metamodel.security.SecuritySchemeModel._
import amf.plugins.domain.webapi.metamodel.security.SecuritySchemeModel
import amf.plugins.domain.webapi.models.{Parameter, Response}
import org.yaml.model.YPart

case class SecurityScheme(fields: Fields, annotations: Annotations)
    extends DomainElement
    with Linkable
    with WithSettings {
  def name: String                    = fields(Name)
  def `type`: String                  = fields(Type)
  def displayName: String             = fields(DisplayName)
  def description: String             = fields(Description)
  def headers: Seq[Parameter]         = fields(Headers)
  def queryParameters: Seq[Parameter] = fields(QueryParameters)
  def responses: Seq[Response]        = fields(Responses)
  def settings: Settings              = fields(SettingsField)
  def queryString: Shape              = fields(QueryString)

  def withName(name: String): this.type                               = set(Name, name)
  def withType(`type`: String): this.type                             = set(Type, `type`)
  def withDisplayName(displayName: String): this.type                 = set(DisplayName, displayName)
  def withDescription(description: String): this.type                 = set(Description, description)
  def withHeaders(headers: Seq[Parameter]): this.type                 = setArray(Headers, headers)
  def withQueryParameters(queryParameters: Seq[Parameter]): this.type = setArray(QueryParameters, queryParameters)
  def withResponses(responses: Seq[Response]): this.type              = setArray(Responses, responses)
  def withSettings(settings: Settings): this.type                     = set(SettingsField, settings)
  def withQueryString(queryString: Shape): this.type                  = set(QueryString, queryString)

  override def adopted(parent: String): this.type = withId(parent + "/" + name)

  def withHeader(name: String): Parameter = {
    val result = Parameter().withName(name)
    add(Headers, result)
    result
  }

  def withQueryParameter(name: String): Parameter = {
    val result = Parameter().withName(name)
    add(QueryParameters, result)
    result
  }

  def withResponse(name: String): Response = {
    val result = Response().withName(name).withStatusCode(if (name == "default") "200" else name)
    add(Responses, result)
    result
  }

  def withDefaultSettings(): Settings = {
    val settings = Settings()
    set(SettingsField, settings)
    settings
  }

  def withOAuth1Settings(): OAuth1Settings = {
    val settings = OAuth1Settings()
    set(SettingsField, settings)
    settings
  }

  def withOAuth2Settings(): OAuth2Settings = {
    val settings = OAuth2Settings()
    set(SettingsField, settings)
    settings
  }

  def withApiKeySettings(): ApiKeySettings = {
    val settings = ApiKeySettings()
    set(SettingsField, settings)
    settings
  }

  def withObject(): ApiKeySettings = {
    val settings = ApiKeySettings()
    set(SettingsField, settings)
    settings
  }

  def cloneScheme(parent: String): SecurityScheme = {
    val cloned = SecurityScheme(annotations).withName(name).adopted(parent)

    this.fields.foreach {
      case (f, v) =>
        val clonedValue = v.value match {
          case s: Settings => s.cloneSettings(cloned.id)
          case a: AmfArray =>
            domain.AmfArray(a.values.map {
              case p: Parameter => p.cloneParameter(cloned.id)
              case r: Response  => r.cloneResponse(cloned.id)
              case o            => o
            }, a.annotations)
          case o => o
        }

        cloned.set(f, clonedValue, v.annotations)
    }

    cloned.asInstanceOf[this.type]
  }

  override def linkCopy(): SecurityScheme = SecurityScheme().withId(id)

  override def meta = SecuritySchemeModel
}

object SecurityScheme {
  def apply(): SecurityScheme = apply(Annotations())

  def apply(ast: YPart): SecurityScheme = apply(Annotations(ast))

  def apply(annotations: Annotations): SecurityScheme = SecurityScheme(Fields(), annotations)
}
