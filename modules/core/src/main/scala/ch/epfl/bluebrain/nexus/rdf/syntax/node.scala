package ch.epfl.bluebrain.nexus.rdf.syntax

import ch.epfl.bluebrain.nexus.rdf.Iri.{AbsoluteIri, Url}
import ch.epfl.bluebrain.nexus.rdf.Node
import ch.epfl.bluebrain.nexus.rdf.Node.{BNode, IriNode, Literal}
import ch.epfl.bluebrain.nexus.rdf.encoder.NodeEncoder.EncoderResult
import ch.epfl.bluebrain.nexus.rdf.encoder.NodeEncoderError.NoElementToEncode
import ch.epfl.bluebrain.nexus.rdf.encoder.{NodeEncoder, NodeEncoderError}

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

  implicit class NodeSyntaxEncoder(node: Node) {

    /**
      * Attempts to convert a [[Node]] to ''A'' using the provided encoder
      *
      * @param encoder the implicitly available encoder
      * @tparam A the generic type to transform the [[Node]] into
      * @return a [[ch.epfl.bluebrain.nexus.rdf.encoder.NodeEncoderError]] when failed and an ''A'' when successful
      */
    def as[A](implicit encoder: NodeEncoder[A]): EncoderResult[A] = encoder(node)
  }

  object encoder {

    implicit class OptionNodeSyntaxEncoder(nodeOpt: Option[Node]) {

      /**
        * Attempts to convert an option of [[Node]] to ''A'' using the provided encoder
        *
        * @param encoder the implicitly available encoder
        * @tparam A the generic type to transform the [[Node]] into
        * @return a [[ch.epfl.bluebrain.nexus.rdf.encoder.NodeEncoderError]] when failed and an ''A'' when successful
        */
      def as[A](implicit encoder: NodeEncoder[A]): EncoderResult[A] =
        nodeOpt.toRight(NoElementToEncode).flatMap(encoder.apply)
    }

    implicit class OptionListNodeSyntaxEncoder(nodeListOpt: Option[Iterable[Node]]) {

      /**
        * Attempts to convert an option of [[Node]] to a List[A] using the provided encoder
        *
        * @param encoder the implicitly available encoder
        * @tparam A the generic type to transform the [[Node]] into
        * @return a [[ch.epfl.bluebrain.nexus.rdf.encoder.NodeEncoderError]] when failed and a List[A] when successful
        */
      def asListOf[A](implicit encoder: NodeEncoder[A]): EncoderResult[List[A]] =
        nodeListOpt.toRight(NoElementToEncode).flatMap { list =>
          val (results, error) = list.foldLeft[(List[A], NodeEncoderError)]((List.empty, NoElementToEncode)) {
            case ((acc, err), c) =>
              encoder(c) match {
                case Left(e)        => acc              -> e
                case Right(encoded) => (encoded :: acc) -> err
              }
          }
          if (results.isEmpty) Left(error)
          else Right(results)
        }
    }

  }
}
