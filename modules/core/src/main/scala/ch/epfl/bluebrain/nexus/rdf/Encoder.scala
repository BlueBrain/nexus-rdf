package ch.epfl.bluebrain.nexus.rdf

import java.time.{Instant, Period}
import java.util.UUID

import cats.{Contravariant, Foldable}
import ch.epfl.bluebrain.nexus.rdf.Graph.{OptionalGraph, SetGraph}
import ch.epfl.bluebrain.nexus.rdf.Iri.AbsoluteIri
import ch.epfl.bluebrain.nexus.rdf.Node.{BNode, IriNode}
import ch.epfl.bluebrain.nexus.rdf.Vocabulary.rdf

import scala.annotation.tailrec
import scala.concurrent.duration.{Duration, FiniteDuration}

/**
  * A type class that provides conversion from values of type ''A'' to an RDF [[Graph]]. Implementation inspired from
  * the circe project (https://github.com/circe/circe).
  */
trait Encoder[A] extends Serializable { self =>

  /**
    * Converts a value to an RDF [[Graph]].
    */
  def apply(a: A): Graph

  /**
    * Creates a new [[Encoder]] by applying a function to a value of type `B` to produce a value of type `A` and then
    * encoding the result using this.
    */
  final def contramap[B](f: B => A): Encoder[B] = new Encoder[B] {
    override def apply(a: B): Graph = self(f(a))
  }
}

object Encoder
    extends PrimitiveEncoderInstances
    with StandardEncoderInstances
    with RdfEncoderInstances
    with LowPriorityEncoderInstances {

  /**
    * Summon an [[Encoder]] for the type `A` from the implicit scope.
    */
  @inline
  final def apply[A](implicit instance: Encoder[A]): Encoder[A] = instance

  /**
    * Constructs an [[Encoder]] from a function.
    */
  final def instance[A](f: A => Graph): Encoder[A] = new Encoder[A] {
    final def apply(a: A): Graph = f(a)
  }

  implicit final val graphEncoderContravariant: Contravariant[Encoder] = new Contravariant[Encoder] {
    override def contramap[A, B](fa: Encoder[A])(f: B => A): Encoder[B] = fa.contramap(f)
  }
}

private[rdf] abstract class IterableAsListEncoder[C[_], A](A: Encoder[A]) extends Encoder[C[A]] {
  protected def toIterator(a: C[A]): Iterator[A]
  override def apply(a: C[A]): Graph = {
    val it = toIterator(a)
    @tailrec
    def inner(acc: Graph, head: A): Graph = {
      if (it.hasNext) {
        val bnode = BNode()
        val g =
          acc
            .append(A(head), IriNode(rdf.first))
            .append(IriNode(rdf.rest), bnode)
            .withNode(bnode)
        inner(g, it.next())
      } else {
        acc
          .append(A(head), IriNode(rdf.first))
          .append(IriNode(rdf.rest), IriNode(rdf.nil))
      }
    }

    if (it.hasNext) {
      val bnode = BNode()
      inner(Graph(bnode), it.next()).withNode(bnode)
    } else Graph(IriNode(rdf.nil))
  }
}

trait PrimitiveEncoderInstances {
  import Encoder.instance
  implicit final val graphEncodeBoolean: Encoder[Boolean] = instance(value => Graph(value))
  implicit final val graphEncodeByte: Encoder[Byte]       = instance(value => Graph(value))
  implicit final val graphEncodeShort: Encoder[Short]     = instance(value => Graph(value))
  implicit final val graphEncodeInt: Encoder[Int]         = instance(value => Graph(value))
  implicit final val graphEncodeLong: Encoder[Long]       = instance(value => Graph(value))
  implicit final val graphEncodeFloat: Encoder[Float]     = instance(value => Graph(value))
  implicit final val graphEncodeDouble: Encoder[Double]   = instance(value => Graph(value))
  implicit final val graphEncodeString: Encoder[String]   = instance(value => Graph(value))
}

trait StandardEncoderInstances {
  import Encoder.instance

  implicit final val graphEncodeUUID: Encoder[UUID]                     = instance(value => Graph(value.toString))
  implicit final val graphEncodeDuration: Encoder[Duration]             = instance(value => Graph(value.toString))
  implicit final val graphEncodeFiniteDuration: Encoder[FiniteDuration] = instance(value => Graph(value.toString))
  implicit final val graphEncodeInstant: Encoder[Instant]               = instance(value => Graph(value.toString))
  implicit final val graphEncodePeriod: Encoder[Period]                 = instance(value => Graph(value.toString))

  implicit final def graphEncodeSet[A](implicit A: Encoder[A]): Encoder[Set[A]] = new Encoder[Set[A]] {
    override def apply(a: Set[A]): Graph = {
      val graphs = a.map(A.apply)
      SetGraph(graphs.headOption.map(_.node).getOrElse(BNode()), graphs)
    }
  }

  implicit final def graphEncodeList[A](implicit A: Encoder[A]): Encoder[List[A]] =
    new IterableAsListEncoder[List, A](A) {
      override protected def toIterator(a: List[A]): Iterator[A] = a.iterator
    }

  implicit final def graphEncodeVector[A](implicit A: Encoder[A]): Encoder[Vector[A]] =
    new IterableAsListEncoder[Vector, A](A) {
      override protected def toIterator(a: Vector[A]): Iterator[A] = a.iterator
    }

  implicit final def graphEncodeArray[A](implicit A: Encoder[A]): Encoder[Array[A]] =
    new IterableAsListEncoder[Array, A](A) {
      override protected def toIterator(a: Array[A]): Iterator[A] = a.iterator
    }

  implicit final def graphEncodeOption[A](implicit A: Encoder[A]): Encoder[Option[A]] = new Encoder[Option[A]] {
    override def apply(a: Option[A]): Graph =
      OptionalGraph(a.map(A.apply))
  }

  implicit final def graphEncodeEither[A, B](implicit A: Encoder[A], B: Encoder[B]): Encoder[Either[A, B]] =
    new Encoder[Either[A, B]] {
      override def apply(a: Either[A, B]): Graph = a match {
        case Left(a)  => A(a)
        case Right(b) => B(b)
      }
    }
}

trait RdfEncoderInstances {
  import Encoder.instance
  implicit final val graphEncodeAbsoluteIri: Encoder[AbsoluteIri] = instance(value => Graph(IriNode(value)))
}

trait LowPriorityEncoderInstances {
  implicit final def encodeIterable[C[_], A](implicit A: Encoder[A], ev: C[A] => Iterable[A]): Encoder[C[A]] =
    new IterableAsListEncoder[C, A](A) {
      override protected def toIterator(a: C[A]): Iterator[A] = ev(a).iterator
    }

  implicit final def encodeFoldable[F[_], A](implicit A: Encoder[A], F: Foldable[F]): Encoder[F[A]] =
    encodeIterable[Vector, A].contramap(fa => F.foldLeft(fa, Vector.empty[A])((acc, el) => acc :+ el))
}
