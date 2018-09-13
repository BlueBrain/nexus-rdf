package ch.epfl.bluebrain.nexus.rdf.bench

import ch.epfl.bluebrain.nexus.rdf.Graph
import ch.epfl.bluebrain.nexus.rdf.Graph._
import ch.epfl.bluebrain.nexus.rdf.Node.IriNode
import ch.epfl.bluebrain.nexus.rdf.Vocabulary._
import ch.epfl.bluebrain.nexus.rdf.syntax.circe._
import ch.epfl.bluebrain.nexus.rdf.syntax.node.unsafe._
import org.scalatest.{Matchers, WordSpecLike}

class GraphOpsSpec extends WordSpecLike with Matchers {
  "A GraphOps" should {
    val json       = jsonContentOf("/schema.json")
    val graph      = json.asGraph.right.get
    val s: IriNode = url"http://example.com/id"
    "return the same graph from all benchmarks" in {
      val bench1 = graph
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

      val triples = graph.triples.filter {
        case (`s`, p, _) =>
          p == rdf.tpe || p == xsd.negativeInteger || p == owl.imports || p == xsd.dateTime || p == xsd.double || p == owl.hasValue || p == owl.oneOf || p == owl.sameAs || p == rdf.first || p == rdf.rest
        case _ => false
      }
      val bench2 = graph -- Graph(triples)

      val bench3 = graph.remove(
        s,
        p =>
          p == rdf.tpe || p == xsd.negativeInteger || p == owl.imports || p == xsd.dateTime || p == xsd.double || p == owl.hasValue || p == owl.oneOf || p == owl.sameAs || p == rdf.first || p == rdf.rest
      )

      bench1 shouldEqual bench2
      bench1 shouldEqual bench3
    }
  }

}
