package ch.epfl.bluebrain.nexus.rdf.jsonld

import cats.Monad
import cats.data.EitherT
import ch.epfl.bluebrain.nexus.rdf.Iri
import ch.epfl.bluebrain.nexus.rdf.Iri.AbsoluteIri
import io.circe.{Json, JsonObject}
import io.circe.syntax._
import cats.implicits._

object JsonLd {

  /**
    * Resolve all IRIs inside @context.
    *
    * @param json     [[Json]] with context to resolve
    * @param resolver context resolver
    * @return [[Json]] with all the context IRIs resolved
    */
  def resolveContext[F[_]: Monad](
      json: Json
  )(resolver: AbsoluteIri => F[Option[Json]]): F[Either[ContextResolutionError, Json]] = {

    def inner(resolvedIds: List[AbsoluteIri], context: Json): EitherT[F, ContextResolutionError, Json] =
      (context.asString, context.asArray, context.asObject) match {
        case (Some(str), _, _) =>
          val nextRef = Iri.absolute(str).toOption
          // format: off
          for {
            next  <- EitherT.fromOption[F](nextRef, IllegalContextValue(str))
            _     <- if (resolvedIds.contains(next)) EitherT.leftT[F, Unit](CircularContextDependency(next :: resolvedIds)) else EitherT.rightT[F, ContextResolutionError](())
            res   <- EitherT.fromOptionF(resolver(next), ContextNotFound(next))
            value <- inner(next :: resolvedIds, contextValue(res))
          } yield value
        // format: on
        case (_, Some(arr), _) =>
          EitherT(arr.traverse(j => inner(resolvedIds, j).value).map {
            _.foldLeft[Either[ContextResolutionError, Json]](Right(Json.obj())) {
              case (Right(accJ), Right(json)) =>
                Right(accJ deepMerge json)
              case (Left(rej), _) => Left(rej)
              case (_, Left(rej)) => Left(rej)
            }
          })

        case (_, _, Some(_)) => EitherT.rightT[F, ContextResolutionError](context)
        case (_, _, _)       => EitherT.leftT[F, Json](IllegalContextValue(context.spaces2): ContextResolutionError)
      }
    inner(List.empty, contextValue(json))
      .map(flattened => replaceContext(json, Json.obj("@context" -> flattened)))
      .value
  }

  /**
    * @return a new Json with the values of all the ''@context'' keys
    */
  def contextValue(json: Json): Json =
    (json.asObject, json.asArray) match {
      case (Some(jObj), _) if jObj.nonEmpty =>
        val context = jObj("@context").getOrElse(Json.obj())
        jObj.remove("@context").values.foldLeft(context)((acc, c) => merge(acc, contextValue(c)))
      case (_, Some(arr)) if arr.nonEmpty =>
        arr.foldLeft(Json.obj())((acc, c) => merge(acc, contextValue(c)))
      case _ =>
        Json.obj()
    }

  /**
    * Replaces the @context value from the provided json to the one in ''that'' json
    *
    * @param json the primary json
    * @param that the json with a @context to override the @context in the provided ''json''
    */
  def replaceContext(json: Json, that: Json): Json =
    removeNestedKeys(json, "@context") deepMerge Json.obj("@context" -> contextValue(that))

  /**
    * Removes the provided keys from everywhere on the json.
    *
    * @param json the json
    * @param keys list of ''keys'' to be removed from the top level of the ''json''
    * @return the original json without the provided ''keys''
    */
  def removeNestedKeys(json: Json, keys: String*): Json = {
    def inner(obj: JsonObject): JsonObject =
      JsonObject.fromIterable(
        obj.filterKeys(!keys.contains(_)).toVector.map { case (k, v) => k -> removeNestedKeys(v, keys: _*) }
      )
    json.arrayOrObject[Json](
      json,
      arr => Json.fromValues(removeEmpty(arr.map(j => removeNestedKeys(j, keys: _*)))),
      obj => inner(obj).asJson
    )
  }

  private def removeEmpty(arr: Seq[Json]): Seq[Json] =
    arr.filter(j => j != Json.obj() && j != Json.fromString("") && j != Json.arr())

  private def merge(json: Json, that: Json): Json =
    (json.asArray, that.asArray, json.asString, that.asString) match {
      case (Some(arr), Some(thatArr), _, _) => Json.arr(removeEmpty(arr ++ thatArr): _*)
      case (_, Some(thatArr), _, _)         => Json.arr(removeEmpty(json +: thatArr): _*)
      case (Some(arr), _, _, _)             => Json.arr(removeEmpty(arr :+ that): _*)
      // $COVERAGE-OFF$
      case (_, _, Some(str), Some(thatStr)) => Json.arr(removeEmpty(Seq(str.asJson, thatStr.asJson)): _*)
      case (_, _, Some(str), _)             => Json.arr(removeEmpty(Seq(str.asJson, that)): _*)
      case (_, _, _, Some(thatStr))         => Json.arr(removeEmpty(Seq(json, thatStr.asJson)): _*)
      // $COVERAGE-ON$
      case _ => json deepMerge that
    }

  /**
    * Exception signalling an error during context resolution.
    * @param msg error message
    */
  @SuppressWarnings(Array("IncorrectlyNamedExceptions"))
  sealed abstract class ContextResolutionError(val msg: String) extends Exception with Product with Serializable {
    override def fillInStackTrace(): ContextResolutionError = this
    // $COVERAGE-OFF$
    override def getMessage: String = msg
    // $COVERAGE-ON$
  }

  /**
    * Exception signalling circular context dependency.
    * @param ids list of context dependencies
    */
  @SuppressWarnings(Array("IncorrectlyNamedExceptions"))
  final case class CircularContextDependency(ids: List[AbsoluteIri])
      extends ContextResolutionError(
        s"Context dependency graph '${ids.reverseIterator.map(_.show).to(List).mkString(" -> ")}' contains a cycle"
      )

  /**
    * Exception signalling illegal context value.
    * @param context the illegal value
    */
  @SuppressWarnings(Array("IncorrectlyNamedExceptions"))
  final case class IllegalContextValue(context: String)
      extends ContextResolutionError(s"'$context' is not a valid @context value")

  /**
    * Exception signalling that a context could not be resolved.
    * @param id context ID
    */
  @SuppressWarnings(Array("IncorrectlyNamedExceptions"))
  final case class ContextNotFound(id: AbsoluteIri)
      extends ContextResolutionError(s"Context ${id.show} could not be resolved.")

}
