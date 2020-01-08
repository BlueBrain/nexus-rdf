package ch.epfl.bluebrain.nexus.rdf.bench

import java.util.UUID

import ch.epfl.bluebrain.nexus.rdf.Iri.AbsoluteIri
import ch.epfl.bluebrain.nexus.rdf.Node.IriNode
import ch.epfl.bluebrain.nexus.rdf.Vocabulary.{nxv => _, rdf, xsd, owl}
import ch.epfl.bluebrain.nexus.rdf.bench.GraphOps._
import ch.epfl.bluebrain.nexus.rdf.jena.Jena
import ch.epfl.bluebrain.nexus.rdf.jena.syntax.all._
import ch.epfl.bluebrain.nexus.rdf.syntax.iri._
import ch.epfl.bluebrain.nexus.rdf.{Graph, Iri}
import io.circe.Json
import io.circe.parser.parse
import org.apache.jena.rdf.model.Model
import org.openjdk.jmh.annotations.{Benchmark, Scope, State}

import scala.io.Source

//noinspection TypeAnnotation
/**
  * Benchmark on Graph operations
  * To run it, execute on the sbt shell: ''jmh:run -i 20 -wi 10 -f1 -t1 .*GraphOps.*''
  * Which means "10 iterations" "10 warmup iterations" "1 fork" "1 thread"
  * Results:
  * Benchmark                                  Mode  Cnt      Score     Error  Units
  * GraphOps.convertGraphToJenaModel  thrpt   20    7355,793 ±  469,653  ops/s
  * GraphOps.convertJenaModelToGraph  thrpt   20     379,893 ±    1,084  ops/s
  * GraphOps.decodeFromGraph          thrpt   20   58856,420 ±  166,960  ops/s
  * GraphOps.parseRemoveFromSet       thrpt   20  129368,036 ± 2808,046  ops/s
  */
@State(Scope.Thread)
class GraphOps {

  val json: Json   = jsonContentOf("/schema.json")
  val s: IriNode   = url"https://bluebrain.github.io/nexus/schemas/resolver"
  val model: Model = Jena.parse(json.spaces2).getOrElse(throw new IllegalArgumentException)
  val graph: Graph = model.asRdfGraph(s).getOrElse(throw new IllegalArgumentException)

  val resolverGraph: Graph = Jena
    .parse(jsonContentOf("/resolver.json").spaces2)
    .flatMap(_.asRdfGraph(url"http://example.com/resolver"))
    .getOrElse(throw new IllegalArgumentException)

  @Benchmark
  def convertGraphToJenaModel(): Unit = {
    val _ = graph.asJena
  }

  @Benchmark
  def convertJenaModelToGraph(): Unit = {
    val _ = model.asRdfGraph(s).getOrElse(throw new IllegalArgumentException)
  }

  @Benchmark
  def decodeFromGraph(): Unit = {
    val c = Graph(resolverGraph.node, resolverGraph.triples).cursor
    val view = for {
      uuid <- c.down(nxv.uuid).as[UUID]
      schemas <- c
        .downSet(nxv.resourceSchemas)
        .as[Set[AbsoluteIri]]
      types <- c
        .downSet(nxv.resourceTypes)
        .as[Set[AbsoluteIri]]
      tag <- c
        .down(nxv.resourceTag)
        .as[Option[String]]
      includeMeta <- c
        .down(nxv.includeMetadata)
        .as[Boolean]
      includeDep <- c
        .down(nxv.includeDeprecated)
        .as[Boolean]
    } yield
      SparqlView(
        schemas,
        types,
        tag,
        includeMeta,
        includeDep,
        uuid
      )
    val _ = view.getOrElse(throw new IllegalArgumentException)
  }

  @Benchmark
  def parseRemoveFromSet(): Unit = {
    val triples = graph.triples.filter {
      case (`s`, p, _) =>
        p.value == rdf.tpe ||
          p.value == xsd.negativeInteger ||
          p.value == owl.imports ||
          p.value == xsd.dateTime ||
          p.value == xsd.double ||
          p.value == owl.hasValue ||
          p.value == owl.oneOf ||
          p.value == owl.sameAs ||
          p.value == rdf.first ||
          p.value == rdf.rest
      case _ => false
    }
    val _ = graph -- triples
  }
}

object GraphOps {
  final case class SparqlView(
      resourceSchemas: Set[AbsoluteIri],
      resourceTypes: Set[AbsoluteIri],
      resourceTag: Option[String],
      includeMetadata: Boolean,
      includeDeprecated: Boolean,
      uuid: UUID
  )
  object nxv {
    val base: Iri.AbsoluteIri = url"https://bluebrain.github.io/nexus/vocabulary/"

    val uuid              = base + "_uuid"
    val resourceSchemas   = base + "resourceSchemas"
    val resourceTypes     = base + "resourceTypes"
    val resourceTag       = base + "resourceTag"
    val includeMetadata   = base + "includeMetadata"
    val includeDeprecated = base + "includeDeprecated"
  }
  implicit def prefixIriToIriNodeF(iri: AbsoluteIri): IriNode => Boolean = _ == IriNode(iri)

  def jsonContentOf(resourcePath: String): Json =
    parse(Source.fromInputStream(getClass.getResourceAsStream(resourcePath)).mkString).toTry
      .getOrElse(throw new IllegalArgumentException)
}
