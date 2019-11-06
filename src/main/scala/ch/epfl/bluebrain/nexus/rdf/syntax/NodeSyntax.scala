package ch.epfl.bluebrain.nexus.rdf.syntax

import cats.implicits._
import ch.epfl.bluebrain.nexus.rdf.Iri.Url
import ch.epfl.bluebrain.nexus.rdf.Node
import ch.epfl.bluebrain.nexus.rdf.Node.{BNode, IriNode}
import ch.epfl.bluebrain.nexus.rdf.encoder.NodeEncoder
import ch.epfl.bluebrain.nexus.rdf.encoder.NodeEncoder.EncoderResult
import ch.epfl.bluebrain.nexus.rdf.encoder.NodeEncoderError.NoElementToEncode

trait NodeSyntax {

  implicit final def nodeEncoder(node: Node): NodeSyntaxEncoder = new NodeSyntaxEncoder(node)

  implicit final def optNodeEncoder(node: Option[Node]): OptionNodeSyntaxEncoder = new OptionNodeSyntaxEncoder(node)

  implicit final def optNodesEncoder(nodes: Option[Iterable[Node]]): OptionNodesSyntaxEncoder =
    new OptionNodesSyntaxEncoder(nodes)

  implicit final def nodeEncoderResult[A](result: EncoderResult[A]): NodeSyntaxEncoderResult[A] =
    new NodeSyntaxEncoderResult(result)
}

trait NodeUnsafeSyntax {

  implicit final def nodeContextSyntax(sc: StringContext): NodeContext = new NodeContext(sc)
}

final class NodeContext(val sc: StringContext) extends AnyVal {
  def b(args: Any*): BNode = {
    val string = sc.s(args: _*)
    BNode(string).getOrElse(throw new IllegalArgumentException(s"Illegal blank node identifier '$string'"))
  }
  def url(args: Any*): IriNode =
    IriNode(Url.unsafe(sc.s(args: _*)))
}

final class NodeSyntaxEncoderResult[A](private val result: EncoderResult[A]) extends AnyVal {

  def orElse(default: => A): EncoderResult[A] = result match {
    case Left(NoElementToEncode) => Right(default)
    case other                   => other
  }
}

final class NodeSyntaxEncoder(private val node: Node) extends AnyVal {

  def as[A](implicit encoder: NodeEncoder[A]): EncoderResult[A] = encoder(node)
}

final class OptionNodeSyntaxEncoder(nodeOpt: Option[Node]) {

  def as[A](implicit encoder: NodeEncoder[A]): EncoderResult[A] =
    nodeOpt.toRight(NoElementToEncode).flatMap(encoder.apply)

  def as[A](default: => A)(implicit encoder: NodeEncoder[A]): EncoderResult[A] =
    asOption[A].map(_.getOrElse(default))

  def asOption[A](implicit encoder: NodeEncoder[A]): EncoderResult[Option[A]] =
    nodeOpt match {
      case None       => Right(None)
      case Some(node) => node.as[A].map(Option(_))
    }
}

final class OptionNodesSyntaxEncoder(nodeListOpt: Option[Iterable[Node]]) {

  def asListOf[A](implicit encoder: NodeEncoder[A]): EncoderResult[Iterable[A]] =
    nodeListOpt
      .toRight(NoElementToEncode)
      .flatMap(
        _.toList.foldM(Vector.empty[A]) {
          case (acc, c) => encoder(c).map(acc :+ _)
        } match {
          case Right(list) if list.isEmpty => Left(NoElementToEncode)
          case Right(list)                 => Right(list)
          case err                         => err
        }
      )
}
