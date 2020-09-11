package amf.plugins.document.webapi

import amf.core.emitter.RenderOptions
import amf.core.model.document._
import amf.plugins.document.webapi.contexts.emitter.OasLikeSpecEmitterContext
import amf.plugins.document.webapi.contexts.parser.OasLikeWebApiContext

import scala.collection.mutable

trait OasLikePlugin extends BaseWebApiPlugin {

  override def specContext(options: RenderOptions): OasLikeSpecEmitterContext

  // We might find $refs in the document pointing to actual shapes in external files in the
  // right positions of the AST.
  // We will try to promote these external fragments to data type fragments instead of just inlining them.
  def promoteFragments(unit: BaseUnit, ctx: OasLikeWebApiContext): BaseUnit = {
    var oldReferences = unit.references.foldLeft(mutable.LinkedHashMap[String, BaseUnit]()) {
      case (acc: mutable.LinkedHashMap[String, BaseUnit], e: BaseUnit) =>
        acc.put(e.location().getOrElse(e.id), e)
        acc
    }
    ctx.declarations.promotedFragments.foreach { promoted =>
      val key = promoted.location().getOrElse(promoted.id)
      oldReferences.put(key, promoted)
    }

    if (oldReferences.values.nonEmpty)
      unit.withReferences(oldReferences.values.toSeq)
    else
      unit
  }

}
