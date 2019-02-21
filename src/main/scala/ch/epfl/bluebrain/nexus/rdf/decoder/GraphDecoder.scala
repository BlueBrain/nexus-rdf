package ch.epfl.bluebrain.nexus.rdf.decoder

import java.io.ByteArrayOutputStream
import java.util.UUID

import ch.epfl.bluebrain.nexus.rdf.{Dot, Node, RootedGraph}
import ch.epfl.bluebrain.nexus.rdf.Node.{BNode, IriNode, IriOrBNode}
import ch.epfl.bluebrain.nexus.rdf.decoder.GraphDecoder.GraphDecoderResult
import ch.epfl.bluebrain.nexus.rdf.decoder.GraphDecoderError.{ConversionError, Unexpected}
import ch.epfl.bluebrain.nexus.rdf.jena.JenaModel
import ch.epfl.bluebrain.nexus.rdf.syntax.JsonLdSyntax
import ch.epfl.bluebrain.nexus.rdf.syntax.node.unsafe._
import com.github.jsonldjava.core.JsonLdOptions
import io.circe.parser.parse
import io.circe.syntax._
import io.circe.{Json, JsonObject}
import org.apache.jena.query.DatasetFactory
import org.apache.jena.rdf.model.Model
import org.apache.jena.riot.system.RiotLib
import org.apache.jena.riot.{JsonLDWriteContext, RDFDataMgr, RDFFormat}
import scalax.collection.edge.LkDiEdge
import scalax.collection.io.dot.Indent.TwoSpaces
import scalax.collection.io.dot._
import scalax.collection.{Graph => G}

import scala.util.Try

/**
  * Defines an decoder from [[RootedGraph]] to ''A''
  */
trait GraphDecoder[A] {

  /**
    * Attempts to transform [[RootedGraph]] to a value of type ''A'' using the provided ''context'' to shorten the keys with its prefix mappings.
    *
    * @param graph the rooted graph to convert into a value of type ''A''
    * @param context the context with it's prefix mappings
    */
  def apply(graph: RootedGraph, context: Json): GraphDecoderResult[A]

  /**
    * Attempts to transform [[RootedGraph]] to a value of type ''A''.
    *
    * @param graph the roted graph to convert into a value of type ''A''
    */
  def apply(graph: RootedGraph): GraphDecoderResult[A] = apply(graph, Json.obj())

}

object GraphDecoder extends JsonLdSyntax {

  type GraphDecoderResult[A] = Either[GraphDecoderError, A]

  final implicit val jenaModelGraphDecoder: GraphDecoder[Model] =
    (graph, _) => Right(JenaModel(graph))

  final implicit val jsonGraphDecoder: GraphDecoder[Json] = new GraphDecoder[Json] {

    private final val reservedId = url"http://dummy.com/${UUID.randomUUID()}"

    override def apply(graph: RootedGraph, context: Json): GraphDecoderResult[Json] = {

      val filteredCtx                    = context.removeContextIris
      val jenaCleanup: JenaWriterCleanup = new JenaWriterCleanup(filteredCtx)

      def writeFramed: GraphDecoderResult[Json] = {
        val opts = new JsonLdOptions()
        opts.setEmbed(true)
        opts.setProcessingMode(JsonLdOptions.JSON_LD_1_1)
        opts.setCompactArrays(true)
        opts.setPruneBlankNodeIdentifiers(true)
        val frame =
          Json.obj("@id" -> Json.fromString(graph.rootNode.toString)).appendContextOf(jenaCleanup.cleanFromCtx)
        val ctx = new JsonLDWriteContext
        ctx.setFrame(frame.noSpaces)
        ctx.setOptions(opts)
        jenaModelGraphDecoder(graph).flatMap { jenamModel =>
          val g   = DatasetFactory.wrap(jenamModel).asDatasetGraph
          val out = new ByteArrayOutputStream()
          val w   = RDFDataMgr.createDatasetWriter(RDFFormat.JSONLD_FRAME_FLAT)
          val pm  = RiotLib.prefixMap(g)
          Try {
            w.write(out, g, pm, null, ctx)
            out.toString
          }.toEither match {
            case Right(jsonString) =>
              val parsedOrErr =
                parse(jsonString).left.map(parsingErr => ConversionError(parsingErr.message, Some(parsingErr)))
              parsedOrErr
                .map(jenaCleanup.removeSingleGraph)
                .map(jenaCleanup.cleanFromJson(_, graph))
                .map(_ deepMerge filteredCtx)
            case Left(message) => Left(Unexpected(s"error while writting Json-LD. Reason '$message'"))
          }
        }
      }

      graph.rootNode match {
        case `reservedId` => writeFramed.map(removeKey(_, "@id"))
        case _: IriNode   => writeFramed
        case blank: BNode => apply(RootedGraph(reservedId, graph.replaceNode(blank, reservedId)), context)
      }
    }

    private def removeKey(json: Json, key: String): Json = {
      def inner(obj: JsonObject): Json = obj.remove(key).asJson
      json.arrayOrObject[Json](json, _.map(removeKey(_, key)).asJson, inner)
    }
  }

  final implicit val dotGraphDecoder: GraphDecoder[Dot] = new GraphDecoder[Dot] {

    private implicit class EdgeGTOps(e: LkDiEdge[G[Node, LkDiEdge]#NodeT]) {
      def s: IriOrBNode = e.from.toOuter.asInstanceOf[IriOrBNode]
      def p: IriNode    = e.label.asInstanceOf[IriNode]
      def o: Node       = e.to.value
    }

    private val root = DotRootGraph(directed = true, id = None)

    private val spacing = Spacing(indent = TwoSpaces, graphAttrSeparator = new AttrSeparator("\n|".stripMargin) {})

    override def apply(graph: RootedGraph, context: Json): GraphDecoderResult[Dot] = {
      def transformer(innerEdge: G[Node, LkDiEdge]#EdgeT): Option[(DotGraph, DotEdgeStmt)] = {
        val edge: LkDiEdge[G[Node, LkDiEdge]#NodeT] = innerEdge.edge
        val (from, label, to)                       = (edge.s.toString, edge.p.toString, edge.to.toString)
        Some(root -> DotEdgeStmt(NodeId(from), NodeId(to), List(DotAttr(Id("label"), Id(label)))))
      }
      val dot = graph.underlying.toDot(dotRoot = root,
                                       edgeTransformer = transformer,
                                       cNodeTransformer = None,
                                       spacing = spacing)
      Right(Dot(dot))
    }
  }
}
