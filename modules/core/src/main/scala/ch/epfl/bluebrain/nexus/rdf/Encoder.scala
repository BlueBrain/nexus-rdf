package ch.epfl.bluebrain.nexus.rdf

import java.util.UUID

import ch.epfl.bluebrain.nexus.rdf.Graph.{OptionalGraph, SetGraph}
import ch.epfl.bluebrain.nexus.rdf.Iri.AbsoluteIri
import ch.epfl.bluebrain.nexus.rdf.Node.{BNode, IriNode}

trait Encoder[A] extends Serializable { self =>

  def apply(a: A): Graph

  final def contramap[B](f: B => A): Encoder[B] = new Encoder[B] {
    override def apply(a: B): Graph = self(f(a))
  }

}

object Encoder extends PrimitiveEncoderInstances with StandardEncoderInstances with RdfEncoderInstances {

  @inline
  final def apply[A](implicit instance: Encoder[A]): Encoder[A] = instance

  final def instance[A](f: A => Graph): Encoder[A] = new Encoder[A] {
    final def apply(a: A): Graph = f(a)
  }
}

trait PrimitiveEncoderInstances {
  import Encoder.instance
  implicit final val graphEncodeBoolean: Encoder[Boolean] = instance(value => Graph(value))
  implicit final val graphEncodeInt: Encoder[Int]         = instance(value => Graph(value))
  implicit final val graphEncodeLong: Encoder[Long]       = instance(value => Graph(value))
  implicit final val graphEncodeFloat: Encoder[Float]     = instance(value => Graph(value))
  implicit final val graphEncodeDouble: Encoder[Double]   = instance(value => Graph(value))
  implicit final val graphEncodeString: Encoder[String]   = instance(value => Graph(value))
}

trait StandardEncoderInstances {
  import Encoder.instance

  implicit final val graphEncodeUUID: Encoder[UUID] = instance(value => Graph(value.toString))

  implicit final def graphEncodeSet[A](implicit A: Encoder[A]): Encoder[Set[A]] = new Encoder[Set[A]] {
    override def apply(a: Set[A]): Graph = {
      val graphs = a.map(A.apply)
      SetGraph(graphs.headOption.map(_.node).getOrElse(BNode()), graphs)
    }
  }

  implicit final def graphEncodeList[A](implicit A: Encoder[A]): Encoder[List[A]] = new Encoder[List[A]] {
    // TODO: make this tail recursive
    override def apply(a: List[A]): Graph = {
      a match {
        case Nil => Graph(IriNode(Vocabulary.rdf.nil))
        case head :: Nil =>
          Graph(BNode())
            .append(A(head), IriNode(Vocabulary.rdf.first))
            .append(IriNode(Vocabulary.rdf.rest), IriNode(Vocabulary.rdf.nil))
        case head :: tail =>
          Graph(BNode())
            .append(A(head), IriNode(Vocabulary.rdf.first))
            .append(apply(tail), IriNode(Vocabulary.rdf.rest))
      }
    }
  }

  implicit final def graphEncodeOption[A](implicit A: Encoder[A]): Encoder[Option[A]] = new Encoder[Option[A]] {
    override def apply(a: Option[A]): Graph =
      OptionalGraph(a.map(A.apply))
  }
}

trait RdfEncoderInstances {
  import Encoder.instance
  implicit final val graphEncodeAbsoluteIri: Encoder[AbsoluteIri] = instance(value => Graph(IriNode(value)))
}
