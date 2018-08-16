package ch.epfl.bluebrain.nexus.rdf.syntax

import ch.epfl.bluebrain.nexus.rdf.Graph
import ch.epfl.bluebrain.nexus.rdf.Graph.Triple
import ch.epfl.bluebrain.nexus.rdf.syntax.JenaSyntax.{JenaGraphSyntax, JenaModelSyntax}
import ch.epfl.bluebrain.nexus.rdf.syntax.jena._
import org.apache.jena.rdf.model.{Model, ModelFactory, ResourceFactory, Statement}

import scala.collection.JavaConverters._

trait JenaSyntax {
  implicit final def jenaGraphSyntax(graph: Graph): JenaGraphSyntax = new JenaGraphSyntax(graph)
  implicit final def jenaModelSyntax(model: Model): JenaModelSyntax = new JenaModelSyntax(model)
}

private[syntax] object JenaSyntax {
  final class JenaGraphSyntax(private val g: Graph) extends AnyVal {
    def asJenaModel: Model = {
      val model = ModelFactory.createDefaultModel()
      val statements = g.triples.foldLeft(Array.empty[Statement]) {
        case (acc, (s, o, p)) => acc :+ ResourceFactory.createStatement(s, o, p)
      }
      model.add(statements)
    }
  }

  final class JenaModelSyntax(private val m: Model) extends AnyVal {
    def asGraph: Graph =
      Graph(m.listStatements().asScala.foldLeft(Set.empty[Triple]) { (acc, s) =>
        acc + ((s.getSubject, s.getPredicate, s.getObject))
      })
  }
}
