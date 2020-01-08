package ch.epfl.bluebrain.nexus.rdf.bench

import java.util.UUID

import ch.epfl.bluebrain.nexus.rdf.Iri.AbsoluteIri
import ch.epfl.bluebrain.nexus.rdf.{Graph, Iri, Resources, RootedGraph}
import ch.epfl.bluebrain.nexus.rdf.Node.IriNode
import ch.epfl.bluebrain.nexus.rdf.Vocabulary._
import ch.epfl.bluebrain.nexus.rdf.bench.GraphOps._
import ch.epfl.bluebrain.nexus.rdf.instances._
import ch.epfl.bluebrain.nexus.rdf.jena.JenaModel
import ch.epfl.bluebrain.nexus.rdf.syntax._
import io.circe.Json
import org.openjdk.jmh.annotations.{Benchmark, Scope, State}

//noinspection TypeAnnotation
/**
  * Benchmark on Graph operations
  * To run it, execute on the sbt shell: ''jmh:run -i 20 -wi 10 -f1 -t1 .*GraphOps.*''
  * Which means "10 iterations" "10 warmup iterations" "1 fork" "1 thread"
  * Results:
  * Benchmark                                  Mode  Cnt      Score     Error  Units
  * GraphOps.convertGraphToJenaModel          thrpt   20   4061,012 ±  81,771  ops/s
  * GraphOps.convertJenaModelToGraph          thrpt   20    270,261 ±   1,241  ops/s
  * GraphOps.decodeFromGraph                  thrpt   20  62423,539 ± 408,686  ops/s
  * GraphOps.parseRemoveFromSet               thrpt   20   1420,970 ±   2,481  ops/s
  * GraphOps.parseRemoveOriginal              thrpt   20    186,158 ±   0,346  ops/s
  * GraphOps.parseRemoveOriginalWithFunction  thrpt   20   1871,456 ± 107,865  ops/s
  */
@State(Scope.Thread)
class GraphOps extends Resources {

  val json: Json         = jsonContentOf("/schema.json")
  val s: IriNode         = url"https://bluebrain.github.io/nexus/schemas/resolver"
  val graph: RootedGraph = json.asGraph(s).getOrElse(throw new IllegalArgumentException)
  val model              = JenaModel(json).getOrElse(throw new IllegalArgumentException)

  val resolverGraph: RootedGraph = jsonContentOf("/resolver.json")
    .asGraph(url"http://example.com/resolver")
    .getOrElse(throw new IllegalArgumentException)

  @Benchmark
  def convertGraphToJenaModel(): Unit = {
    val _ = JenaModel(graph)
  }

  @Benchmark
  def convertJenaModelToGraph(): Unit = {
    val _ = model.asGraph(s).getOrElse(throw new IllegalArgumentException)
  }

  @Benchmark
  def decodeFromGraph(): Unit = {
    val c = resolverGraph.cursor()
    val view = for {
      uuid <- c.downField(nxv.uuid).focus.as[UUID]
      schemas <- c
        .downField(nxv.resourceSchemas)
        .values
        .asListOf[AbsoluteIri]
        .map(_.toSet)
      types <- c
        .downField(nxv.resourceTypes)
        .values
        .asListOf[AbsoluteIri]
        .map(_.toSet)
      tag <- c
        .downField(nxv.resourceTag)
        .focus
        .asOption[String]
      includeMeta <- c
        .downField(nxv.includeMetadata)
        .focus
        .as[Boolean]
      includeDep <- c
        .downField(nxv.includeDeprecated)
        .focus
        .as[Boolean]
    } yield SparqlView(
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
  def parseRemoveOriginal(): Unit = {
    val _ = graph
      .remove(s, rdf.tpe)
      .remove(s, xsd.negativeInteger)
      .remove(s, owl.imports)
      .remove(s, xsd.dateTime)
      .remove(s, xsd.double)
      .remove(s, owl.hasValue)
      .remove(s, owl.oneOf)
      .remove(s, owl.sameAs)
      .remove(s, rdf.first)
      .remove(s, rdf.rest)
  }

  @Benchmark
  def parseRemoveFromSet(): Unit = {
    val triples = graph.triples.filter {
      case (`s`, p, _) =>
        p == rdf.tpe || p == xsd.negativeInteger || p == owl.imports || p == xsd.dateTime || p == xsd.double || p == owl.hasValue || p == owl.oneOf || p == owl.sameAs || p == rdf.first || p == rdf.rest
      case _ => false
    }
    val _ = graph -- Graph(triples)
  }

  @Benchmark
  def parseRemoveOriginalWithFunction(): Unit = {
    val _ = graph.remove(
      s,
      p =>
        p == rdf.tpe ||
          p == xsd.negativeInteger ||
          p == owl.imports ||
          p == xsd.dateTime ||
          p == xsd.double ||
          p == owl.hasValue ||
          p == owl.oneOf ||
          p == owl.sameAs ||
          p == rdf.first ||
          p == rdf.rest
    )
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
    val base: Iri.AbsoluteIri = url"https://bluebrain.github.io/nexus/vocabulary/".value

    val uuid              = base + "_uuid"
    val resourceSchemas   = base + "resourceSchemas"
    val resourceTypes     = base + "resourceTypes"
    val resourceTag       = base + "resourceTag"
    val includeMetadata   = base + "includeMetadata"
    val includeDeprecated = base + "includeDeprecated"
  }
  implicit def prefixIriToIriNodeF(iri: AbsoluteIri): IriNode => Boolean = _ == IriNode(iri)
}
