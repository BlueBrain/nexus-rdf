package ch.epfl.bluebrain.nexus.rdf.syntax

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}

import cats.instances.all._
import cats.syntax.all._
import ch.epfl.bluebrain.nexus.rdf.Iri.AbsoluteIri
import ch.epfl.bluebrain.nexus.rdf.Node.{IriNode, IriOrBNode}
import ch.epfl.bluebrain.nexus.rdf.Vocabulary._
import ch.epfl.bluebrain.nexus.rdf.syntax.GraphSyntax._
import ch.epfl.bluebrain.nexus.rdf.syntax.circe.context._
import ch.epfl.bluebrain.nexus.rdf.syntax.jena._
import ch.epfl.bluebrain.nexus.rdf.{Graph, Iri}
import com.github.jsonldjava.core.JsonLdOptions
import io.circe._
import io.circe.parser.parse
import io.circe.syntax._
import org.apache.jena.query.DatasetFactory
import org.apache.jena.rdf.model.{Model, ModelFactory}
import org.apache.jena.riot.system.RiotLib
import org.apache.jena.riot.{JsonLDWriteContext, Lang, RDFDataMgr, RDFFormat}

import scala.util.Try

trait GraphSyntax {

  implicit final def graphSyntax(graph: Graph): GraphOps = new GraphOps(graph)

  implicit final def circeSyntax(json: Json): CirceOps = new CirceOps(json)
}

private[syntax] object GraphSyntax {
  val knownTypes: Set[String] =
    Set(
      xsd.boolean.show,
      xsd.int.show,
      xsd.integer.show,
      xsd.string.show,
      xsd.decimal.show,
      xsd.double.show,
      xsd.float.show,
      xsd.long.show,
      xsd.string.show,
      xsd.dateTime.show
    )

  def model(json: Json): Model = {
    val model     = ModelFactory.createDefaultModel()
    val finalJson = json deepMerge json.removeContextIris
    RDFDataMgr.read(model, new ByteArrayInputStream(finalJson.noSpaces.getBytes), Lang.JSONLD)
    model
  }

}

final class CirceOps(private val json: Json) extends AnyVal {

  /**
    * Convert Json-LD into [[Graph]]
    *
    * @return [[Graph]] object created from given JSON-LD
    */
  def asGraph: Graph = model(json)

  /**
    * Attempts to find the top `@id` value
    * @return Some(iri) of found, None otherwise
    */
  def id: Option[AbsoluteIri] = {
    val m = model(json)

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

  /**
    * Convert [[Graph]] into JSON-LD representation. Value of `@context` will be generated by Jena
    *
    * @return [[Json]] containing JSON-LD representation of the [[Graph]]
    */
  def asJson: Json = {
    val out = new ByteArrayOutputStream()
    RDFDataMgr.write(out, graph, RDFFormat.JSONLD)
    parse(out.toString).getOrElse(Json.obj())
  }

  /**
    * Convert [[Graph]] into JSON-LD representation using provided context. Beware, that currently IRI contexts are
    * not resolved and will be ignored.
    *
    * @param id      the optionally provided initial entity @id
    * @param context context to use when creating JSON-LD representation
    * @return [[Json]] containing JSON-LD representation of the [[Graph]]
    */
  def asJson(context: Json, id: Option[IriOrBNode]): Try[Json] = {
    val filteredCtx                             = context.removeContextIris
    implicit val jenaCleanup: JenaWriterCleanup = new JenaWriterCleanup(filteredCtx)
    id match {
      case Some(iri: IriNode) => writeFramed(filteredCtx, iri)
      case _                  => write(filteredCtx)
    }
  }

  private def writeFramed(c: Json, id: IriNode)(implicit jenaCleanup: JenaWriterCleanup): Try[Json] = {
    val opts = new JsonLdOptions()
    opts.setEmbed(true)
    opts.setProcessingMode(JsonLdOptions.JSON_LD_1_1)
    opts.setCompactArrays(true)
    opts.setPruneBlankNodeIdentifiers(true)
    val frame = Json.obj("@id" -> Json.fromString(id.show)).appendContextOf(jenaCleanup.cleanFromCtx)
    val ctx   = new JsonLDWriteContext
    ctx.setFrame(frame.noSpaces)
    ctx.setOptions(opts)
    write(RDFFormat.JSONLD_FRAME_FLAT, ctx)(jenaCleanup).map(_ deepMerge c)
  }

  private def write(c: Json)(implicit jenaCleanup: JenaWriterCleanup): Try[Json] = {
    val opts = new JsonLdOptions()
    val ctx  = new JsonLDWriteContext
    ctx.setJsonLDContext(jenaCleanup.cleanFromCtx.noSpaces)
    ctx.setOptions(opts)
    write(RDFFormat.JSONLD, ctx)(jenaCleanup).map(_ deepMerge c)
  }

  private def write(format: RDFFormat, ctx: JsonLDWriteContext)(implicit jenaCleanup: JenaWriterCleanup): Try[Json] = {
    val g   = DatasetFactory.wrap(graph).asDatasetGraph
    val out = new ByteArrayOutputStream()
    val w   = RDFDataMgr.createDatasetWriter(format)
    val pm  = RiotLib.prefixMap(g)
    Try {
      w.write(out, g, pm, null, ctx)
    }.flatMap(_ => parse(out.toString).map(jenaCleanup.removeSingleGraph).map(jenaCleanup.cleanFromJson).toTry)
  }
}

private[syntax] final class JenaWriterCleanup(ctx: Json) {
  private lazy val m = model(ctx)

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
    * @param json the json to be cleaned
    */
  private[syntax] def cleanFromJson(json: Json): Json = {
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
      JsonObject.fromIterable(jObj.toVector.map { case (k, v) => k -> inner(v) }).asJson

    def deleteType(jObj: JsonObject): Json =
      if (jObj.contains("@type") && jObj.contains("@value") && jObj.size == 2)
        (jObj("@type").flatMap(_.asString).map(m.expandPrefix), jObj("@value"))
          .mapN {
            case (t, value) if t == xsd.boolean.show  => tryBoolean(value)
            case (t, value) if t == xsd.int.show      => tryInt(value)
            case (t, value) if t == xsd.integer.show  => tryInt(value)
            case (t, value) if t == xsd.long.show     => tryLong(value)
            case (t, value) if t == xsd.float.show    => tryFloat(value)
            case (t, value) if t == xsd.decimal.show  => tryDouble(value)
            case (t, value) if t == xsd.double.show   => tryDouble(value)
            case (t, value) if t == xsd.dateTime.show => value.asString.map(Json.fromString)
            case (t, value) if t == xsd.string.show   => value.asString.map(Json.fromString)
            case _                                    => None
          }
          .flatten
          .getOrElse(recursiveFollow(jObj))
      else
        recursiveFollow(jObj)

    inner(json)
  }

}
