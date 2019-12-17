package ch.epfl.bluebrain.nexus.rdf.jena.syntax

import ch.epfl.bluebrain.nexus.rdf.Node.{BNode, IriNode, IriOrBNode, Literal}
import ch.epfl.bluebrain.nexus.rdf.{Graph, Node}
import org.apache.jena.rdf.model
import org.apache.jena.rdf.model.{Model, Property, RDFNode, Resource}

trait ToJenaSyntax {
  import ch.epfl.bluebrain.nexus.rdf.jena.{JenaConverters => conv}

  implicit class AsJenaGraph(graph: Graph) {

    /**
      * Converts a [[ch.epfl.bluebrain.nexus.rdf.Graph]] to a mutable Jena [[org.apache.jena.rdf.model.Model]]. The
      * conversion is lossy as the graph anchor is lost.
      */
    def asJena: Model =
      conv.asJena(graph)
  }

  implicit class AsJenaNode(node: Node) {

    /**
      * Converts a [[ch.epfl.bluebrain.nexus.rdf.Node]] to a Jena [[org.apache.jena.rdf.model.RDFNode]].
      */
    def asJena: RDFNode =
      conv.asJena(node)
  }

  implicit class AsJenaIriOrBNode(node: IriOrBNode) {

    /**
      * Converts a [[ch.epfl.bluebrain.nexus.rdf.Node.IriOrBNode]] to a Jena [[org.apache.jena.rdf.model.Resource]].
      */
    def asJena: Resource =
      conv.asJena(node)
  }

  implicit class AsJenaIriNode(node: IriNode) {

    /**
      * Converts a [[ch.epfl.bluebrain.nexus.rdf.Node.IriNode]] to a Jena [[org.apache.jena.rdf.model.Property]].
      */
    def asJena: Property =
      conv.asJena(node)
  }

  implicit class AsJenaBNode(node: BNode) {

    /**
      * Converts a [[ch.epfl.bluebrain.nexus.rdf.Node.BNode]] to a Jena [[org.apache.jena.rdf.model.Resource]].
      */
    def asJena: Resource =
      conv.asJena(node)
  }

  implicit class AsJenaLiteral(node: Literal) {

    /**
      * Converts a [[ch.epfl.bluebrain.nexus.rdf.Node.BNode]] to a Jena [[org.apache.jena.rdf.model.Literal]].
      */
    def asJena: model.Literal =
      conv.asJena(node)
  }

}
