package amf.resolution.stages
import amf.ProfileNames
import amf.framework.model.document.{BaseUnit, Document}
import amf.domain.Parameter
import amf.metadata.domain.{EndPointModel, RequestModel}
import amf.model.AmfArray
import amf.plugins.domain.webapi.metamodel.WebApiModel
import amf.plugins.domain.webapi.models.WebApi
import amf.validation.Validation

import scala.collection.mutable

/**
  * Place parameter models in the right locations according to the RAML/OpenAPI specs and our own
  * criterium for AMF
  * @param profile target profile
  */
class ParametersNormalizationStage(profile: String)(override implicit val currentValidation: Validation) extends ResolutionStage(profile) {

  val paramsAcc: mutable.HashMap[String, Seq[Parameter]] = mutable.HashMap()

  override def resolve(model: BaseUnit): BaseUnit = {
    profile match {
      case ProfileNames.RAML => parametersRaml(model)
      case ProfileNames.OAS  => parametersOpenApi(model)
      case ProfileNames.AMF  => parametersAmf(model)
      case _                 => throw new Exception(s"Unknown profile $profile")
    }
  }

  /**
    * In AMF we push all the parameters at the operation level.
    * Parameter references should be already resolved in previous steps.
    * @param unit BaseUnit in
    * @return unit BaseUnit out
    */
  protected def parametersAmf(unit: BaseUnit): BaseUnit = {
    paramsAcc.clear()
    unit match {
      case doc: Document if doc.encodes.isInstanceOf[WebApi] =>
        // collect baseUri paraemters
        val webapi            = doc.encodes.asInstanceOf[WebApi]
        val baseUriParameters = Option(webapi.baseUriParameters).getOrElse(Seq())
        webapi.fields.remove(WebApiModel.BaseUriParameters)
        // collect endpoint path parameters
        webapi.endPoints.foreach { endpoint =>
          val endpointParameters = Option(endpoint.parameters).getOrElse(Seq())
          endpoint.fields.remove(EndPointModel.UriParameters)
          // collect operation query parameters
          if (baseUriParameters.nonEmpty || endpointParameters.nonEmpty)
            endpoint.operations.foreach { op =>
              Option(op.request) match {
                case Some(request) =>
                  val queryParameters = request.queryParameters
                  // set the full list of parameters at the operation level
                  request.fields.setWithoutId(RequestModel.QueryParameters,
                                              AmfArray(baseUriParameters ++ endpointParameters ++ queryParameters))
                case _ => // ignore
              }
            }
        }
        doc
      case _ => unit
    }
  }

  /**
    * In OpenAPI we just push the endpoint parameters to the operation level, overwriting the any endpoint parameter
    * with the new definition at the operation level
    * @param unit BaseUnit in
    * @return unit BaseUnit out
    */
  protected def parametersOpenApi(unit: BaseUnit): BaseUnit = {
    paramsAcc.clear()
    unit match {
      case doc: Document if doc.encodes.isInstanceOf[WebApi] =>
        val webapi = doc.encodes.asInstanceOf[WebApi]
        // collect endpoint path parameters
        webapi.endPoints.foreach { endpoint =>
          val endpointParameters = Option(endpoint.parameters).getOrElse(Seq())
          endpoint.fields.remove(EndPointModel.UriParameters)
          // collect operation query parameters
          if (endpointParameters.nonEmpty)
            endpoint.operations.foreach { op =>
              Option(op.request) match {
                case Some(request) =>
                  val queryParameters = request.queryParameters
                  // set the full list of parameters at the operation level
                  val finalParameters = disambiguateParameters(endpointParameters, queryParameters)
                  request.fields.setWithoutId(RequestModel.QueryParameters, AmfArray(finalParameters))
                case _ => // ignore
              }
            }
        }
        doc
      case _ => unit
    }
  }

  /**
    * In RAML we assign the parameters at the right level according to the RAML spec:
    * - webapi for baseURI parameters
    * - endpoint for the path parameters
    * - operation for the  query parameters
    * Since parameters can be at any level due to the source of the model being an OpenAPI document
    * @param unit BaseUnit in
    * @return unit BaseUnit out
    */
  protected def parametersRaml(unit: BaseUnit): BaseUnit = {
    paramsAcc.clear()
    unit match {
      case doc: Document if doc.encodes.isInstanceOf[WebApi] =>
        val webapi = doc.encodes.asInstanceOf[WebApi]
        // collect endpoint path parameters
        webapi.endPoints.foreach { endpoint =>
          val endpointParameters = Option(endpoint.parameters).getOrElse(Seq())
          endpoint.fields.remove(EndPointModel.UriParameters)
          // we filter path parameters and the remaining parameters
          val pathParameters      = endpointParameters.filter(p => p.binding == "path")
          val pathQueryParameters = endpointParameters.filter(p => p.binding != "path")
          // we re-assign path parametes at the endpoint, we push the rest
          if (pathParameters.nonEmpty)
            endpoint.fields.setWithoutId(EndPointModel.UriParameters, AmfArray(pathParameters))
          // collect operation query parameters
          if (pathQueryParameters.nonEmpty)
            endpoint.operations.foreach { op =>
              Option(op.request) match {
                case Some(request) =>
                  val opParameters = request.queryParameters
                  // set the full list of parameters at the operation level
                  val finalParameters = disambiguateParameters(pathQueryParameters, opParameters)
                  request.fields.setWithoutId(RequestModel.QueryParameters, AmfArray(finalParameters))
                case _ => // ignore
              }
            }
        }
        doc
      case _ => unit
    }
  }

  protected def disambiguateParameters(left: Seq[Parameter], right: Seq[Parameter]): Seq[Parameter] = {
    val params: mutable.HashMap[String, Parameter] = mutable.HashMap()
    left.foreach(p => params.put(p.name + p.binding, p))
    right.foreach(p => params.put(p.name + p.binding, p))
    params.values.toSeq
  }
}
