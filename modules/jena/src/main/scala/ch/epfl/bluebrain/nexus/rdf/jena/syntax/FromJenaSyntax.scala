package ch.epfl.bluebrain.nexus.rdf.jena.syntax

import ch.epfl.bluebrain.nexus.rdf.{Graph, Node}
import org.apache.jena.rdf.model
import org.apache.jena.rdf.model.{Model, RDFNode, Resource}

trait FromJenaSyntax {
  import ch.epfl.bluebrain.nexus.rdf.jena.{JenaConverters => conv}

  implicit class AsRdfModel(model: Model) {

    /**
      * Converts a Jena [[org.apache.jena.rdf.model.Model]] to a [[ch.epfl.bluebrain.nexus.rdf.Graph]] using the
      * argument `node` as an anchor.
      *
      * @param node the graph anchor
      */
    def asRdfGraph(node: Node): Either[String, Graph] =
      conv.asRdfGraph(node, model)
  }

  implicit class AsRdfResource(resource: Resource) {

    /**
      * Converts a Jena [[org.apache.jena.rdf.model.Resource]] to an [[ch.epfl.bluebrain.nexus.rdf.Node.IriNode]] if the
      * resource is an IRI.
      */
    def asRdfIriNode: Either[String, Node.IriNode] =
      conv.asRdfIriNode(resource)

    /**
      * Converts a Jena [[org.apache.jena.rdf.model.Resource]] to an [[ch.epfl.bluebrain.nexus.rdf.Node.IriOrBNode]] if
      * the resource is an IRI or a blank node.
      */
    def asRdfIriOrBNode: Either[String, Node.IriOrBNode] =
      conv.asRdfIriOrBNode(resource)

    /**
      * Converts a Jena [[org.apache.jena.rdf.model.Resource]] to an [[ch.epfl.bluebrain.nexus.rdf.Node.BNode]] if the
      * resource is a blank node.
      */
    def asRdfBNode: Either[String, Node.BNode] =
      conv.asRdfBNode(resource)
  }

  implicit class AsRdfLiteral(literal: model.Literal) {

    /**
      * Converts a Jena [[org.apache.jena.rdf.model.Literal]] to an [[ch.epfl.bluebrain.nexus.rdf.Node.Literal]].
      */
    def asRdfLiteral: Either[String, Node.Literal] =
      conv.asRdfLiteral(literal)
  }

  implicit class AsRdfNode(node: RDFNode) {

    /**
      * Converts a Jena [[org.apache.jena.rdf.model.RDFNode]] to an [[ch.epfl.bluebrain.nexus.rdf.Node]].
      */
    def asRdfNode: Either[String, Node] =
      conv.asRdfNode(node)
  }
}
