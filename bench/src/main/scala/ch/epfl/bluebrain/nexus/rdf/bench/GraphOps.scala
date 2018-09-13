package ch.epfl.bluebrain.nexus.rdf.bench

import ch.epfl.bluebrain.nexus.rdf.Graph
import ch.epfl.bluebrain.nexus.rdf.Graph._
import ch.epfl.bluebrain.nexus.rdf.Node.IriNode
import ch.epfl.bluebrain.nexus.rdf.Vocabulary._
import ch.epfl.bluebrain.nexus.rdf.syntax.circe._
import ch.epfl.bluebrain.nexus.rdf.syntax.node.unsafe._
import org.openjdk.jmh.annotations.{Benchmark, Scope, State}

//noinspection TypeAnnotation
/**
  * Benchmark on Graph operations
  * To run it, execute on the sbt shell: ''jmh:run -i 20 -wi 10 -f1 -t1 .*GraphOps.*''
  * Which means "10 iterations" "10 warmup iterations" "1 fork" "1 thread"
  */
@State(Scope.Thread)
class GraphOps {

  val json       = jsonContentOf("/schema.json")
  val graph      = json.asGraph.right.get
  val s: IriNode = url"http://example.com/id"

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
