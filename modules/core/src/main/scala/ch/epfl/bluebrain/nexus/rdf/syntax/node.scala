package ch.epfl.bluebrain.nexus.rdf.syntax

import ch.epfl.bluebrain.nexus.rdf.Iri.{AbsoluteIri, Url}
import ch.epfl.bluebrain.nexus.rdf.Node.{BNode, IriNode, Literal}

object node {

  final implicit def toIriNode(iri: AbsoluteIri): IriNode =
    IriNode(iri)

  final implicit def toLiteral(value: String): Literal =
    Literal(value)

  final implicit def toLiteral(value: Int): Literal =
    Literal(value)

  final implicit def toLiteral(value: Boolean): Literal =
    Literal(value)

  object unsafe {
    implicit class NodeContext(val sc: StringContext) extends AnyVal {
      def b(args: Any*): BNode = {
        val string = sc.s(args: _*)
        BNode(string).getOrElse(throw new IllegalArgumentException(s"Illegal blank node identifier '$string'"))
      }
      def url(args: Any*): IriNode =
        IriNode(Url.unsafe(sc.s(args: _*)))
    }
  }
}
