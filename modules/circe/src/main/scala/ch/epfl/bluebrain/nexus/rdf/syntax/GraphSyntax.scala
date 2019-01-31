package ch.epfl.bluebrain.nexus.rdf.syntax

import java.io.ByteArrayOutputStream
import java.util.UUID

import cats.implicits._
import ch.epfl.bluebrain.nexus.rdf.Iri.AbsoluteIri
import ch.epfl.bluebrain.nexus.rdf.Node.{BNode, IriNode, IriOrBNode, Literal}
import ch.epfl.bluebrain.nexus.rdf.Vocabulary._
import ch.epfl.bluebrain.nexus.rdf.circe.JenaModel
import ch.epfl.bluebrain.nexus.rdf.circe.JenaModel.JenaModelErr
import ch.epfl.bluebrain.nexus.rdf.circe.JenaModel.JenaModelErr.InvalidJsonLD
import ch.epfl.bluebrain.nexus.rdf.syntax.GraphSyntax._
import ch.epfl.bluebrain.nexus.rdf.syntax.circe.context._
import ch.epfl.bluebrain.nexus.rdf.syntax.jena._
import ch.epfl.bluebrain.nexus.rdf.syntax.node.unsafe._
import ch.epfl.bluebrain.nexus.rdf.{Graph, GraphConfiguration, Iri}
import com.github.jsonldjava.core.JsonLdOptions
import io.circe._
import io.circe.parser.parse
import io.circe.syntax._
import org.apache.jena.query.DatasetFactory
import org.apache.jena.rdf.model.ModelFactory
import org.apache.jena.riot.system.RiotLib
import org.apache.jena.riot.{JsonLDWriteContext, RDFDataMgr, RDFFormat}

import scala.util.Try

trait GraphSyntax {

  implicit final def graphSyntax(graph: Graph): GraphOps = new GraphOps(graph)
  implicit final def circeSyntax(json: Json): CirceOps   = new CirceOps(json)
}

private[syntax] object GraphSyntax {
  val knownTypes: Set[String] =
    Set(
      xsd.boolean.toString,
      xsd.int.toString,
      xsd.integer.toString,
      xsd.string.toString,
      xsd.decimal.toString,
      xsd.double.toString,
      xsd.float.toString,
      xsd.long.toString,
      xsd.string.toString,
      xsd.dateTime.toString
    )

  final val reservedId = url"http://dummy.com/${UUID.randomUUID()}"

}

final class CirceOps(private val json: Json) extends AnyVal {

  /**
    * Convert Json-LD into [[Graph]]
    *
    * @return [[Graph]] object created from given JSON-LD
    */
  def asGraph(
      implicit config: GraphConfiguration = GraphConfiguration(castDateTypes = true)): Either[JenaModelErr, Graph] =
    JenaModel(json).flatMap(_.asGraph.left.map(InvalidJsonLD))

  /**
    * Attempts to find the top `@id` value
    * @return Some(iri) of found, None otherwise
    */
  def id: Option[AbsoluteIri] =
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
          inner(json, json.contextValue) orElse singleGraph(json).flatMap(value => inner(value, json.contextValue))
        case (_, Some(arr)) =>
          singleElem(arr).flatMap { head =>
            inner(head, head.contextValue) orElse singleGraph(head).flatMap(value => inner(value, head.contextValue))
          }
        case (_, _) => None
      }
    }
}

final class GraphOps(private val graph: Graph) extends AnyVal {
  import ch.epfl.bluebrain.nexus.rdf.syntax.circe._

  /**
    * Convert [[Graph]] into JSON-LD representation. Value of `@context` will be generated by Jena
    *
    * @return [[Json]] containing JSON-LD representation of the [[Graph]]
    */
  def asJson: Json = {
    val out = new ByteArrayOutputStream()
    RDFDataMgr.write(out, graph.asJenaModel, RDFFormat.JSONLD)
    parse(out.toString).getOrElse(Json.obj())
  }

  /**
    * Convert [[Graph]] into JSON-LD expanded representation.
    *
    * @param id the provided initial entity @id
    * @return [[Json]] containing JSON-LD representation of the [[Graph]]
    */
  def asExpandedJson(id: IriOrBNode): Try[Json] =
    asJson(Json.obj(), id)

  /**
    * Convert [[Graph]] into JSON-LD representation using provided context. Beware, that currently IRI contexts are
    * not resolved and will be ignored.
    *
    * @param id      the provided initial entity @id
    * @param context context to use when creating JSON-LD representation
    * @return [[Json]] containing JSON-LD representation of the [[Graph]]
    */
  def asJson(context: Json, id: IriOrBNode): Try[Json] = {
    val filteredCtx                             = context.removeContextIris
    implicit val jenaCleanup: JenaWriterCleanup = new JenaWriterCleanup(filteredCtx)
    id match {
      case `reservedId` => writeFramed(filteredCtx, reservedId).map(removeKey(_, "@id"))
      case iri: IriNode => writeFramed(filteredCtx, iri)
      case blank: BNode => graph.replaceNode(blank, reservedId).asJson(context, reservedId)
    }
  }

  private def removeKey(json: Json, key: String): Json = {
    def inner(obj: JsonObject): Json = obj.remove(key).asJson
    json.arrayOrObject[Json](json, _.map(removeKey(_, key)).asJson, inner)
  }

  private def writeFramed(c: Json, id: IriNode)(implicit jenaCleanup: JenaWriterCleanup): Try[Json] = {
    val opts = new JsonLdOptions()
    opts.setEmbed(true)
    opts.setProcessingMode(JsonLdOptions.JSON_LD_1_1)
    opts.setCompactArrays(true)
    opts.setPruneBlankNodeIdentifiers(true)
    val frame = Json.obj("@id" -> Json.fromString(id.toString)).appendContextOf(jenaCleanup.cleanFromCtx)
    val ctx   = new JsonLDWriteContext
    ctx.setFrame(frame.noSpaces)
    ctx.setOptions(opts)
    write(RDFFormat.JSONLD_FRAME_FLAT, ctx)(jenaCleanup).map(_ deepMerge c)
  }

  private def write(format: RDFFormat, ctx: JsonLDWriteContext)(implicit jenaCleanup: JenaWriterCleanup): Try[Json] = {
    val g   = DatasetFactory.wrap(graph.asJenaModel).asDatasetGraph
    val out = new ByteArrayOutputStream()
    val w   = RDFDataMgr.createDatasetWriter(format)
    val pm  = RiotLib.prefixMap(g)
    Try {
      w.write(out, g, pm, null, ctx)
    }.flatMap(_ =>
      parse(out.toString).map(jenaCleanup.removeSingleGraph).map(jenaCleanup.cleanFromJson(_, graph)).toTry)
  }
}

private[syntax] final class JenaWriterCleanup(ctx: Json) {
  private lazy val m = JenaModel(ctx).getOrElse(ModelFactory.createDefaultModel())

  /**
    * Removes the "@graph" from the json document if there is only
    * one element inside the array object containing the @graph key
    *
    * @param json the json to be cleaned
    */
  def removeSingleGraph(json: Json): Json =
    (json.hcursor.downField("@graph").focus.flatMap(_.asArray).flatMap {
      case head +: IndexedSeq() => Some(head)
      case _                    => None
    }, json.asObject) match {
      case (Some(entity), Some(obj)) => entity deepMerge obj.remove("@graph").asJson
      case _                         => json
    }

  /**
    * Work around for Jena to replace the keys which are in a context with @type
    * and using the UseNativeTypes flag in the JsonLdOptions
    */
  def cleanFromCtx: Json = {

    def inner(ctx: Json): Json =
      ctx.arrayOrObject(ctx, arr => Json.fromValues(arr.map(inner)), obj => deleteType(obj).asJson)

    def deleteType(jObj: JsonObject): JsonObject =
      JsonObject.fromIterable(
        jObj.toVector
          .filter {
            case ("@type", j) => j.asString.forall(s => !knownTypes.contains(m.expandPrefix(s)))
            case _            => true
          }
          .map { case (k, v) => k -> inner(v) })

    inner(ctx)
  }

  /**
    * Work around for Jena to replace the expanded generated objects into the compacted form
    * For example, the following json payload:
    * {{{
    * "age": {
    *    "@type": "xsd:integer",
    *    "@value": "2"
    * }
    * }}}
    * will be converted to: "age": 2
    *
    * It also deals with relative Iris caused @base, expanding them when required
    *
    * @param json the json to be cleaned
    */
  private[syntax] def cleanFromJson(json: Json, g: Graph): Json = {
    val stringValues = g.triples.collect { case (_, _, lt: Literal) => lt.lexicalForm }
    val maybeBase    = json.contextValue.hcursor.get[String]("@base").flatMap(Iri.absolute)

    def inner(ctx: Json): Json =
      ctx.arrayOrObject(ctx, arr => Json.fromValues(arr.map(inner)), obj => deleteType(obj))

    def tryBoolean(j: Json) =
      if (j.isBoolean) Some(j) else j.asString.flatMap(s => Try(s.toBoolean).toOption).map(Json.fromBoolean)

    def tryInt(j: Json) =
      j.asNumber.flatMap(_.toInt).orElse(j.asString.flatMap(s => Try(s.toInt).toOption)).map(Json.fromInt)

    def tryLong(j: Json) =
      j.asNumber.flatMap(_.toLong).orElse(j.asString.flatMap(s => Try(s.toLong).toOption)).map(Json.fromLong)

    def tryDouble(j: Json) =
      j.asNumber.map(_.toDouble).orElse(j.asString.flatMap(s => Try(s.toDouble).toOption)).flatMap(Json.fromDouble)

    def tryFloat(j: Json) =
      j.asNumber
        .map(_.toDouble)
        .map(_.toFloat)
        .orElse(j.asString.flatMap(s => Try(s.toFloat).toOption))
        .flatMap(Json.fromFloat)

    def recursiveFollow(jObj: JsonObject): Json =
      JsonObject
        .fromIterable(jObj.toVector.map {
          case (k, v) =>
            k -> (v.asString match {
              case Some(s) if s.startsWith("../") && !stringValues.contains(s) =>
                expandWithBase(s).map(Json.fromString).getOrElse(inner(v))
              case _ =>
                inner(v)
            })
        })
        .asJson

    def expandWithBase(s: String) =
      (maybeBase -> Iri.relative(s.substring(3))).mapN { (base, relative) =>
        relative.resolve(base).asString
      }

    def deleteType(jObj: JsonObject): Json =
      if (jObj.contains("@type") && jObj.contains("@value") && jObj.size == 2)
        (jObj("@type").flatMap(_.asString).map(m.expandPrefix), jObj("@value"))
          .mapN {
            case (t, value) if t == xsd.boolean.toString  => tryBoolean(value)
            case (t, value) if t == xsd.int.toString      => tryInt(value)
            case (t, value) if t == xsd.integer.toString  => tryInt(value)
            case (t, value) if t == xsd.long.toString     => tryLong(value)
            case (t, value) if t == xsd.float.toString    => tryFloat(value)
            case (t, value) if t == xsd.decimal.toString  => tryDouble(value)
            case (t, value) if t == xsd.double.toString   => tryDouble(value)
            case (t, value) if t == xsd.dateTime.toString => value.asString.map(Json.fromString)
            case (t, value) if t == xsd.string.toString   => value.asString.map(Json.fromString)
            case _                                        => None
          }
          .flatten
          .getOrElse(recursiveFollow(jObj))
      else
        recursiveFollow(jObj)

    inner(json)
  }

}
