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

final class NodeSyntaxEncoder(private val node: Node) extends AnyVal {

  def as[A](implicit encoder: NodeEncoder[A]): EncoderResult[A] = encoder(node)
}

final class OptionNodeSyntaxEncoder(nodeOpt: Option[Node]) {

  def as[A](implicit encoder: NodeEncoder[A]): EncoderResult[A] =
    nodeOpt.toRight(NoElementToEncode).flatMap(encoder.apply)
}

final class OptionNodesSyntaxEncoder(nodeListOpt: Option[Iterable[Node]]) {

  def asListOf[A](implicit encoder: NodeEncoder[A]): EncoderResult[List[A]] =
    nodeListOpt.toRight(NoElementToEncode).flatMap { list =>
      list.toList.foldM(List.empty[A]) {
        case (acc, c) => encoder(c).map(v => v :: acc)
      } match {
        case Right(Nil)     => Left(NoElementToEncode)
        case Right(encoded) => Right(encoded)
        case err            => err
      }
    }
}
