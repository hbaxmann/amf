package amf.client.plugins

import amf.core.client.ParsingOptions
import amf.core.parser.{ParsedDocument, ParserContext}
import org.mulesoft.common.io.Output

abstract class AMFSyntaxPlugin extends AMFPlugin {
  def supportedMediaTypes(): Seq[String]
  def parse(mediaType: String,
            text: CharSequence,
            ctx: ParserContext,
            parsingOptions: ParsingOptions): Option[ParsedDocument]
  def unparse(mediaType: String, ast: ParsedDocument): Option[CharSequence]
  def unparse[W: Output](mediaType: String, ast: ParsedDocument, writer: W): Option[W]
}
