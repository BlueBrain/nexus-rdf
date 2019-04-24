package ch.epfl.bluebrain.nexus.rdf.circe

import ch.epfl.bluebrain.nexus.rdf.Iri
import ch.epfl.bluebrain.nexus.rdf.Iri.AbsoluteIri
import ch.epfl.bluebrain.nexus.rdf.jena.JenaModel
import io.circe.syntax._
import io.circe.{Json, JsonObject}

object JsonLd {

  /**
    * Adds @id value to the provided Json
    *
    * @param json  the json
    * @param value the @id value
    */
  def id(json: Json, value: AbsoluteIri): Json =
    json.asObject -> json.asArray match {
      case (Some(_), _)                      => json deepMerge Json.obj("@id" -> Json.fromString(value.asString))
      case (_, Some(jArr)) if jArr.size == 1 => Json.arr(id(jArr.head, value))
    }

  /**
    * Attempts to find the top `@id` value on the provided json.
    *
    * @param json the json
    * @return Some(iri) of found, None otherwise
    */
  def id(json: Json): Option[AbsoluteIri] =
    JenaModel(json).toOption.flatMap { m =>
      val aliases = contextAliases(json, "@id") + "@id"

      def singleGraph(value: JsonObject): Option[JsonObject] =
        value("@graph").flatMap { json =>
          json.asObject -> json.asArray match {
            case (Some(jObj), _)                   => Some(jObj)
            case (_, Some(jArr)) if jArr.size == 1 => jArr.head.asObject
            case _                                 => None
          }
        }

      def inner(value: JsonObject): Option[AbsoluteIri] =
        aliases.foldLeft(None: Option[AbsoluteIri]) {
          case (None, alias) => value(alias).flatMap(_.asString).flatMap(s => Iri.absolute(m.expandPrefix(s)).toOption)
          case (iri, _)      => iri
        }

      (json.asObject, json.asArray) match {
        case (Some(jObj), _) =>
          inner(jObj) orElse singleGraph(jObj).flatMap(inner)
        case (_, Some(arr)) if arr.size == 1 =>
          arr.head.asObject.flatMap(jObj => inner(jObj) orElse singleGraph(jObj).flatMap(inner))
        case _ => None
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
    * Retrieves the aliases on the provided ''json'' @context for the provided ''keyword''
    *
    * @param json    the Json-LD
    * @param keyword the Json-LD keyword. E.g.: @id, @type, @container, @set
    * @return a set of aliases found for the given keyword
    */
  def contextAliases(json: Json, keyword: String): Set[String] = {
    val jsonKeyword = Json.fromString(keyword)
    def inner(ctx: Json): Set[String] =
      ctx.asObject -> ctx.asArray match {
        case (Some(jObj), _) =>
          jObj.toMap.collect { case (k, `jsonKeyword`) => k }.toSet
        case (_, Some(jArr)) =>
          jArr.foldLeft(Set.empty[String])(_ ++ inner(_))
        case _ => Set.empty
      }

    inner(contextValue(json))
  }

  /**
    * @return a new Json with the values of the top ''@context'' key
    */
  def contextValue(json: Json): Json =
    json.asObject -> json.asArray match {
      case (Some(jObj), _)                   => jObj("@context").getOrElse(Json.obj())
      case (_, Some(jArr)) if jArr.size == 1 => contextValue(jArr.head)
      case _                                 => Json.obj()
    }

  private def merge(json: Json, that: Json): Json = (json.asArray, that.asArray) match {
    case (Some(arr), Some(thatArr)) => Json.arr(arr ++ thatArr: _*)
    case (_, Some(thatArr))         => Json.arr(json +: thatArr: _*)
    case (Some(arr), _)             => Json.arr(arr :+ that: _*)
    case _                          => json deepMerge that
  }

  /**
    * @param json the primary context. E.g.: {"@context": {...}}
    * @param that the other context from where to merge this context with. E.g.: {"@context": {...}}
    * @return a new Json with the values of the top ''@context'' key (this) and the provided ''that'' top ''@context'' key
    *         If two keys inside both contexts collide, the one in the ''other'' context will override the one in this context
    */
  def mergeContext(json: Json, that: Json): Json =
    Json.obj("@context" -> merge(contextValue(json), contextValue(that)))

  /**
    * @param json the primary context
    * @param that the context to append to this json. E.g.: {"@context": {...}}
    * @return a new Json with the original context plus the provided context
    */
  def appendContextOf(json: Json, that: Json): Json = json deepMerge mergeContext(json, that)

  /**
    * Replaces the @context value from the provided json to the one in ''that'' json
    *
    * @param json the primary json
    * @param that the json with a @context to override the @context in the provided ''json''
    */
  def replaceContext(json: Json, that: Json): Json =
    removeKeys(json, "@context") deepMerge Json.obj("@context" -> contextValue(that))

  /**
    * Replaces the @context value from the provided json to the provided ''iri''
    *
    * @param json the primary json
    * @param iri  the iri which overrides the existing json
    */
  def replaceContext(json: Json, iri: AbsoluteIri): Json =
    removeKeys(json, "@context") deepMerge Json.obj("@context" -> Json.fromString(iri.asString))

  /**
    * Removes the provided keys from the json.
    *
    * @param json the json
    * @param keys list of ''keys'' to be removed from the top level of the ''json''
    * @return the original json without the provided ''keys'' on the top level of the structure
    */
  def removeKeys(json: Json, keys: String*): Json = {
    def inner(obj: JsonObject): Json =
      keys.foldLeft(obj)((accObj, key) => accObj.remove(key)).asJson

    json.arrayOrObject[Json](json, arr => arr.map(j => removeKeys(j, keys: _*)).asJson, obj => inner(obj))
  }

  /**
    * Filter out context which are strings/iris as Jena doesn't  handle them. Other invalid contexts(booleans, numbers) etc.
    * will by handled by Jena and cause an error.
    *
    * @param json the json
    * @return the context in the form {"@context": {...}}. The values that are not inside the key @context are dropped
    */
  def removeContextIris(json: Json): Json = {
    if (json == Json.obj()) json
    else {
      val ctx = contextValue(json)
      (ctx.asString, ctx.asArray) match {
        case (Some(_), _) => Json.obj("@context" -> Json.obj())
        case (_, Some(arr)) =>
          arr.filterNot(_.isString) match {
            case jsonObj +: IndexedSeq() => Json.obj("@context" -> jsonObj)
            case IndexedSeq()            => Json.obj("@context" -> Json.obj())
            case jsonArray               => Json.obj("@context" -> Json.arr(jsonArray: _*))
          }
        case _ => Json.obj("@context" -> ctx)
      }
    }
  }
}
