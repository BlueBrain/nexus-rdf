package ch.epfl.bluebrain.nexus.rdf.bench

import ch.epfl.bluebrain.nexus.rdf.{Graph, Resources, RootedGraph}
import ch.epfl.bluebrain.nexus.rdf.Node.IriNode
import ch.epfl.bluebrain.nexus.rdf.Vocabulary._
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
  * Benchmark              Mode  Cnt     Score     Error  Units
  * Parsing.parseAkkaUri  thrpt   10  1011,101 ± 412,756  ops/s
  * Parsing.parseIri      thrpt   10   219,526 ±  35,304  ops/s
  * Parsing.parseJenaIri  thrpt   10  1178,370 ± 289,768  ops/s
  */
@State(Scope.Thread)
class GraphOps extends Resources {

  val json: Json         = jsonContentOf("/schema.json")
  val s: IriNode         = url"https://bluebrain.github.io/nexus/schemas/resolver"
  val graph: RootedGraph = json.asGraph(s).getOrElse(throw new IllegalArgumentException)
  val model              = JenaModel(json).getOrElse(throw new IllegalArgumentException)

  @Benchmark
  def convertGraphToJenaModel(): Unit = {
    val _ = JenaModel(graph)
  }

  @Benchmark
  def convertJenaModelToGraph(): Unit = {
    val _ = model.asGraph(s).getOrElse(throw new IllegalArgumentException)

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
