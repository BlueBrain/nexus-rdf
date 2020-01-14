package ch.epfl.bluebrain.nexus.rdf.jsonld

import cats.Monad
import cats.data.EitherT
import ch.epfl.bluebrain.nexus.rdf.Iri
import ch.epfl.bluebrain.nexus.rdf.Iri.AbsoluteIri
import io.circe.Json
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
  )(resolver: AbsoluteIri => F[Option[Json]]): EitherT[F, ContextResolutionException, Json] = {

    def inner(resolvedIds: List[AbsoluteIri], context: Json): EitherT[F, ContextResolutionException, Json] =
      (context.asString, context.asArray, context.asObject) match {
        case (Some(str), _, _) =>
          val nextRef = Iri.absolute(str).toOption
          // format: off
          for {
            next  <- EitherT.fromOption[F](nextRef, IllegalContextValue(str))
            _     <- if (resolvedIds.contains(next)) EitherT.leftT[F, Unit](CircularContextDependency(next :: resolvedIds)) else EitherT.rightT[F, ContextResolutionException](())
            res   <- resolveOrNotFound(next)
            value <- inner(next :: resolvedIds, contextValue(res))
          } yield value
        // format: on
        case (_, Some(arr), _) =>
          EitherT(arr.traverse(j => inner(resolvedIds, j).value).map {
            _.foldLeft[Either[ContextResolutionException, Json]](Right(Json.obj())) {
              case (Right(accJ), Right(json)) =>
                Right(accJ deepMerge json)
              case (Left(rej), _) => Left(rej)
              case (_, Left(rej)) => Left(rej)
            }
          })

        case (_, _, Some(_)) => EitherT.rightT[F, ContextResolutionException](context)
        case (_, _, _)       => EitherT.leftT[F, Json](IllegalContextValue(context.spaces2): ContextResolutionException)
      }
    def resolveOrNotFound(ref: AbsoluteIri): EitherT[F, ContextResolutionException, Json] =
      EitherT.fromOptionF(resolver(ref), ContextNotFound(ref))

    inner(List.empty, contextValue(json)).map(flattened => json deepMerge Json.obj("@context" -> flattened))
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

  private def removeEmpty(arr: Seq[Json]): Seq[Json] =
    arr.filter(j => j != Json.obj() && j != Json.fromString("") && j != Json.arr())

  private def merge(json: Json, that: Json): Json =
    (json.asArray, that.asArray, json.asString, that.asString) match {
      case (Some(arr), Some(thatArr), _, _) => Json.arr(removeEmpty(arr ++ thatArr): _*)
      case (_, Some(thatArr), _, _)         => Json.arr(removeEmpty(json +: thatArr): _*)
      case (Some(arr), _, _, _)             => Json.arr(removeEmpty(arr :+ that): _*)
      case (_, _, Some(str), Some(thatStr)) => Json.arr(removeEmpty(Seq(str.asJson, thatStr.asJson)): _*)
      case (_, _, Some(str), _)             => Json.arr(removeEmpty(Seq(str.asJson, that)): _*)
      case (_, _, _, Some(thatStr))         => Json.arr(removeEmpty(Seq(json, thatStr.asJson)): _*)
      case _                                => json deepMerge that
    }

  /**
    * Exception signalling an error during context resolution.
    * @param msg error message
    */
  sealed abstract class ContextResolutionException(val msg: String) extends Exception with Product with Serializable {
    override def fillInStackTrace(): ContextResolutionException = this
    override def getMessage: String                             = msg
  }

  /**
    * Exception signalling circular context dependency.
    * @param ids list of context dependencies
    */
  @SuppressWarnings(Array("IncorrectlyNamedExceptions"))
  final case class CircularContextDependency(ids: List[AbsoluteIri])
      extends ContextResolutionException(
        s"Context dependency graph '${ids.reverseIterator.map(_.show).to(List).mkString(" -> ")}' contains a cycle"
      )

  /**
    * Exception signalling illegal context value.
    * @param context the illegal value
    */
  @SuppressWarnings(Array("IncorrectlyNamedExceptions"))
  final case class IllegalContextValue(context: String)
      extends ContextResolutionException(s"'$context' is not a valid @context value")

  /**
    * Exception signalling that a context could not be resolved.
    * @param id context ID
    */
  @SuppressWarnings(Array("IncorrectlyNamedExceptions"))
  final case class ContextNotFound(id: AbsoluteIri)
      extends ContextResolutionException(s"Context ${id.show} could not be resolved.")

}
