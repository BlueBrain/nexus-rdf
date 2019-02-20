package ch.epfl.bluebrain.nexus.rdf.circe

import ch.epfl.bluebrain.nexus.rdf.Iri
import ch.epfl.bluebrain.nexus.rdf.Iri.AbsoluteIri
import ch.epfl.bluebrain.nexus.rdf.jena.JenaModel
import io.circe.Json

object JsonLd {

  /**
    * Attempts to find the top `@id` value on the provided json.
    *
    * @param json the json
    * @return Some(iri) of found, None otherwise
    */
  def id(json: Json): Option[AbsoluteIri] =
    JenaModel(json).toOption.flatMap { m =>
      def singleGraph(value: Json) =
        value.hcursor.downField("@graph").focus.flatMap(_.asArray).flatMap(singleElem)

      def singleElem(array: Vector[Json]) = array match {
        case head +: IndexedSeq() => Some(head)
        case _                    => None
      }

      def inner(value: Json, ctx: Json): Option[AbsoluteIri] = {

        def asExpandedIri(key: String) =
          value.hcursor.get[String](key).flatMap(s => Iri.absolute(m.expandPrefix(s))).toOption

        def tryOthers: Option[AbsoluteIri] =
          ctx.asObject.flatMap(_.toMap.foldLeft[Option[AbsoluteIri]](None) {
            case (acc @ Some(_), _) => acc
            case (_, (k, v))        => v.asString.withFilter(_ == "@id").flatMap(_ => asExpandedIri(k))
          })

        asExpandedIri("@id") orElse tryOthers
      }

      (json.asObject, json.asArray) match {
        case (Some(_), _) =>
          inner(json, contextValue(json)) orElse singleGraph(json).flatMap(value => inner(value, contextValue(json)))
        case (_, Some(arr)) =>
          singleElem(arr).flatMap { head =>
            inner(head, contextValue(head)) orElse singleGraph(head).flatMap(value => inner(value, contextValue(head)))
          }
        case (_, _) => None
      }
    }

  /**
    * Adds or merges a context URI to an existing JSON object.
    *
    * @param json    the json
    * @param context the standard context URI
    * @return a new JSON object
    */
  def addContext(json: Json, context: AbsoluteIri): Json = {
    val contextUriString = Json.fromString(context.asString)

    json.asObject match {
      case Some(jo) =>
        val updated = jo("@context") match {
          case None => jo.add("@context", contextUriString)
          case Some(value) =>
            (value.asObject, value.asArray, value.asString) match {
              case (Some(vo), _, _) if vo.isEmpty =>
                jo.add("@context", contextUriString)
              case (_, Some(va), _) if va.isEmpty =>
                jo.add("@context", contextUriString)
              case (_, _, Some(vs)) if vs.isEmpty =>
                jo.add("@context", contextUriString)
              case (Some(vo), _, _) if !vo.values.exists(_ == contextUriString) =>
                jo.add("@context", Json.arr(value, contextUriString))
              case (_, Some(va), _) if !va.contains(contextUriString) =>
                jo.add("@context", Json.fromValues(va :+ contextUriString))
              case (_, _, Some(vs)) if vs != context.asString =>
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
  def contextValue(json: Json): Json = json.hcursor.get[Json]("@context").getOrElse(Json.obj())

  /**
    * @param json the primary context. E.g.: {"@context": {...}}
    * @param that the other context from where to merge this context with. E.g.: {"@context": {...}}
    * @return a new Json with the values of the top ''@context'' key (this) and the provided ''that'' top ''@context'' key
    *         If two keys inside both contexts collide, the one in the ''other'' context will override the one in this context
    */
  def mergeContext(json: Json, that: Json): Json =
    Json.obj("@context" -> (contextValue(json) deepMerge contextValue(that)))

  /**
    * @param json the primary context
    * @param that the context to append to this json. E.g.: {"@context": {...}}
    * @return a new Json with the original context plus the provided context
    */
  def appendContextOf(json: Json, that: Json): Json = json deepMerge mergeContext(json, that)

  /**
    * Filter out context which are strings/iris as Jena doesn't  handle them. Other invalid contexts(booleans, numbers) etc.
    * will by handled by Jena and cause an error.
    *
    * @param json the json
    * @return the context in the form {"@context": {...}}
    */
  def removeContextIris(json: Json): Json = {
    if (json == Json.obj()) json
    else {
      val ctx = contextValue(json)
      (ctx.asString, ctx.asArray) match {
        case (Some(_), _)   => Json.obj("@context" -> Json.obj())
        case (_, Some(arr)) => Json.obj("@context" -> Json.arr(arr.filterNot(_.isString): _*))
        case _              => Json.obj("@context" -> ctx)
      }
    }
  }

}
