package ch.epfl.bluebrain.nexus.rdf.decoder

import java.io.ByteArrayOutputStream
import java.util.UUID

import cats.Id
import cats.implicits._
import ch.epfl.bluebrain.nexus.rdf.MarshallingError.{ConversionError, Unexpected}
import ch.epfl.bluebrain.nexus.rdf.Node.{BNode, IriNode, IriOrBNode}
import ch.epfl.bluebrain.nexus.rdf._
import ch.epfl.bluebrain.nexus.rdf.jena.JenaModel
import ch.epfl.bluebrain.nexus.rdf.syntax.{JsonLdSyntax, _}
import com.github.jsonldjava.core.JsonLdOptions
import io.circe.Json
import io.circe.parser.parse
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
trait GraphDecoder[F[_], A] {

  /**
    * Attempts to transform [[RootedGraph]] to a value of type ''A'' using the provided ''context'' to shorten the keys with its prefix mappings.
    *
    * @param graph the rooted graph to convert into a value of type ''A''
    * @param context the context with it's prefix mappings
    */
  def apply(graph: RootedGraph, context: Json): F[A]

  /**
    * Attempts to transform [[RootedGraph]] to a value of type ''A''.
    *
    * @param graph the roted graph to convert into a value of type ''A''
    */
  def apply(graph: RootedGraph): F[A] = apply(graph, Json.obj())

}

object GraphDecoder extends JsonLdSyntax {

  type DecoderResult[A] = Either[MarshallingError, A]

  final implicit val jenaModelGraphDecoder: GraphDecoder[Id, Model] =
    (graph, _) => JenaModel(graph)

  final implicit val jsonGraphDecoder: GraphDecoder[DecoderResult, Json] =
    new GraphDecoder[DecoderResult, Json] {

      private final val reservedId = url"http://dummy.com/${UUID.randomUUID()}"

      override def apply(graph: RootedGraph, context: Json): DecoderResult[Json] = {

        val filteredCtx                    = context.removeContextIris
        val jenaCleanup: JenaWriterCleanup = new JenaWriterCleanup(filteredCtx)

        def writeFramed: DecoderResult[Json] = {
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

        if (graph.triples.isEmpty)
          Right(Json.obj())
        else
          graph.rootNode match {
            case `reservedId` => writeFramed.map(_.removeKeys("@id"))
            case _: IriNode   => writeFramed
            case blank: BNode => apply(RootedGraph(reservedId, graph.replaceNode(blank, reservedId)), context)
          }
      }
    }

  final implicit val dotGraphDecoder: GraphDecoder[Id, Dot] = new GraphDecoder[Id, Dot] {

    private implicit class EdgeGTOps(e: LkDiEdge[G[Node, LkDiEdge]#NodeT]) {
      def s: IriOrBNode = e.from.toOuter.asInstanceOf[IriOrBNode]
      def p: IriNode    = e.label.asInstanceOf[IriNode]
      def o: Node       = e.to.value
    }

    private val root = DotRootGraph(directed = true, id = None)

    private val spacing = Spacing(indent = TwoSpaces, graphAttrSeparator = new AttrSeparator("\n|".stripMargin) {})

    override def apply(graph: RootedGraph, context: Json): Id[Dot] = {
      def transformer(innerEdge: G[Node, LkDiEdge]#EdgeT): Option[(DotGraph, DotEdgeStmt)] = {
        val edge: LkDiEdge[G[Node, LkDiEdge]#NodeT] = innerEdge.edge
        val (from, label, to)                       = (edge.s.toString, edge.p.toString, edge.to.toString)
        Some(root -> DotEdgeStmt(NodeId(from), NodeId(to), List(DotAttr(Id("label"), Id(label)))))
      }
      val dot = graph.underlying.toDot(dotRoot = root,
                                       edgeTransformer = transformer,
                                       cNodeTransformer = None,
                                       spacing = spacing)
      Dot(dot)
    }
  }

  final implicit val ntriplesGraphDecoder: GraphDecoder[Id, NTriples] = (graph, _) =>
    NTriples(graph.triples.map { case (s, p, o) => s"${s.show} ${p.show} ${o.show} ." }.mkString("\n"))

}
