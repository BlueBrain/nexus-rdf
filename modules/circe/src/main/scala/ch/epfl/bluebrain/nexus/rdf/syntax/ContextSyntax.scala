package ch.epfl.bluebrain.nexus.rdf.syntax

import ch.epfl.bluebrain.nexus.rdf.Iri.AbsoluteIri
import ch.epfl.bluebrain.nexus.rdf.syntax.circe.context._
import io.circe.Json
import cats.syntax.show._

trait ContextSyntax {
  implicit final def contextSyntax(json: Json): ContextOps = new ContextOps(json)
}

final class ContextOps(private val json: Json) extends AnyVal {

  /**
    * Adds or merges a context URI to an existing JSON object.
    *
    * @param context the standard context URI
    * @return a new JSON object
    */
  def addContext(context: AbsoluteIri): Json = {
    val contextUriString = Json.fromString(context.show)

    json.asObject match {
      case Some(jo) =>
        val updated = jo("@context") match {
          case None => jo.add("@context", contextUriString)
          case Some(value) =>
            (value.asObject, value.asArray, value.asString) match {
              case (Some(vo), _, _) if !vo.values.exists(_ == contextUriString) =>
                jo.add("@context", Json.arr(value, contextUriString))
              case (_, Some(va), _) if !va.contains(contextUriString) =>
                jo.add("@context", Json.fromValues(va :+ contextUriString))
              case (_, _, Some(vs)) if vs != context.show =>
                jo.add("@context", Json.arr(value, contextUriString))
              case _ => jo
            }
        }
        Json.fromJsonObject(updated)
      case None => json
    }
  }

  /**
    * @return a new Json with the values of the top ''@context'' key
    */
  def contextValue: Json = json.hcursor.get[Json]("@context").getOrElse(Json.obj())

  /**
    * @param that the other context from where to merge this context with
    * @return a new Json with the values of the top ''@context'' key (this) and the provided ''that'' top ''@context'' key
    *         If two keys inside both contexts collide, the one in the ''other'' context will override the one in this context
    */
  def mergeContext(that: Json): Json =
    Json.obj("@context" -> (contextValue deepMerge that.contextValue))

  /**
    * @param that the context to append to this json
    * @return a new Json with the original context plus the context on the provided (''that'') json
    */
  def appendContextOf(that: Json): Json = json deepMerge mergeContext(that)

  /**
    * Filter out context which are strings/iris as Jena doesn't  handle them. Other invalid contexts(booleans, numbers) etc.
    * will by handled by Jena and cause an error.
    *
    * @return the context in the form {"@context": {...}}
    */
  def removeContextIris(): Json = {
    val ctx = json.contextValue
    (ctx.asString, ctx.asArray) match {
      case (Some(_), _)   => Json.obj("@context" -> Json.obj())
      case (_, Some(arr)) => Json.obj("@context" -> Json.arr(arr.filterNot(_.isString): _*))
      case _              => Json.obj("@context" -> ctx)
    }
  }
}
