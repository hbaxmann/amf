package amf.client

import amf.core.remote.Oas
import amf.core.remote.Syntax.Json

import scala.scalajs.js.annotation.JSExportTopLevel

/**
  * [[Oas]] parser.
  */
@JSExportTopLevel("OasParser")
class OasParser extends BaseParser(Oas, Json)
