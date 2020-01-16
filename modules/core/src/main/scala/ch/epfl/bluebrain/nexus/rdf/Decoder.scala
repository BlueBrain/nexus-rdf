package ch.epfl.bluebrain.nexus.rdf

import java.time.{Instant, Period}
import java.util.UUID

import cats.{MonadError, SemigroupK}
import cats.implicits._
import ch.epfl.bluebrain.nexus.rdf.Decoder.Result
import ch.epfl.bluebrain.nexus.rdf.Iri.{AbsoluteIri, Url, Urn}
import ch.epfl.bluebrain.nexus.rdf.Node.{BNode, IriNode, IriOrBNode, Literal}
import ch.epfl.bluebrain.nexus.rdf.Vocabulary.rdf

import scala.annotation.tailrec
import scala.collection.Factory
import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.reflect.ClassTag
import scala.util.Try

/**
  * A type class that produces a value of type `A` from a [[Graph]]. Implementation inspired from the circe project
  * (https://github.com/circe/circe).
  */
trait Decoder[A] extends Serializable { self =>

  /**
    * Decodes the argument [[Cursor]].
    */
  def apply(cursor: Cursor): Result[A]

  /**
    * Creates a new Decoder by mapping the argument function over the result of this Decoder.
    */
  final def map[B](f: A => B): Decoder[B] = new Decoder[B] {
    final def apply(cursor: Cursor): Result[B] =
      self(cursor).map(f)
  }

  /**
    * Binds the argument function over this Decoder.
    */
  final def flatMap[B](f: A => Decoder[B]): Decoder[B] = new Decoder[B] {
    final def apply(cursor: Cursor): Result[B] = self(cursor) match {
      case Right(a)    => f(a)(cursor)
      case l @ Left(_) => l.asInstanceOf[Result[B]]
    }
  }

  /**
    * If this Decoder is successful return its result or fallback on the argument Decoder.
    */
  final def or[AA >: A](d: => Decoder[AA]): Decoder[AA] = new Decoder[AA] {
    final def apply(cursor: Cursor): Result[AA] =
      self(cursor) match {
        case Left(_)      => d(cursor)
        case r @ Right(_) => r
      }
  }

  /**
    * If this Decoder is successful applies the argument function to the result producing either a error message or
    * a new result.
    */
  final def emap[B](f: A => Either[String, B]): Decoder[B] = new Decoder[B] {
    final def apply(c: Cursor): Decoder.Result[B] =
      self(c) match {
        case Right(a) =>
          f(a) match {
            case r @ Right(_)  => r.asInstanceOf[Result[B]]
            case Left(message) => Left(DecodingError(message, c.history))
          }
        case l @ Left(_) => l.asInstanceOf[Result[B]]
      }
  }

  /**
    * Runs both decoders (self and the provided decoder) and returns the result as a pair.
    */
  final def product[B](db: Decoder[B]): Decoder[(A, B)] = new Decoder[(A, B)] {
    override def apply(cursor: Cursor): Result[(A, B)] =
      self.flatMap(a => db.map(b => (a, b)))(cursor)
  }

  /**
    * If this Decoder is successful return its result or recover using the provided function.
    */
  final def handleErrorWith(f: DecodingError => Decoder[A]): Decoder[A] = new Decoder[A] {
    override def apply(cursor: Cursor): Result[A] =
      self(cursor) match {
        case Left(err)    => f(err)(cursor)
        case r @ Right(_) => r
      }
  }

}

object Decoder extends PrimitiveDecoderInstances with StandardDecoderInstances with RdfDecoderInstances {

  /**
    * The Decoder result type.
    */
  final type Result[A] = Either[DecodingError, A]

  /**
    * Summon a [[Decoder]] for the type `A` from the implicit scope.
    */
  @inline
  final def apply[A](implicit instance: Decoder[A]): Decoder[A] = instance

  /**
    * Constructs a [[Decoder]] from a function.
    */
  final def instance[A](f: Cursor => Result[A]): Decoder[A] = new Decoder[A] {
    final def apply(c: Cursor): Result[A] = f(c)
  }

  /**
    * Constructs a [[Decoder]] that always succeeds with the provided value.
    */
  final def const[A](a: A): Decoder[A] = new Decoder[A] {
    override def apply(cursor: Cursor): Result[A] = Right(a)
  }

  /**
    * Constructs a failed [[Decoder]] using the provided error.
    */
  final def failed[A](error: DecodingError): Decoder[A] = new Decoder[A] {
    override def apply(cursor: Cursor): Result[A] = Left(error)
  }

  implicit final val graphDecodeCursor: Decoder[Cursor] = instance(Right(_))

  // ported directly from circe (https://github.com/circe/circe)
  implicit final val decoderInstances: SemigroupK[Decoder] with MonadError[Decoder, DecodingError] =
    new SemigroupK[Decoder] with MonadError[Decoder, DecodingError] {
      final def combineK[A](x: Decoder[A], y: Decoder[A]): Decoder[A]                   = x.or(y)
      final def pure[A](a: A): Decoder[A]                                               = const(a)
      override final def map[A, B](fa: Decoder[A])(f: A => B): Decoder[B]               = fa.map(f)
      override final def product[A, B](fa: Decoder[A], fb: Decoder[B]): Decoder[(A, B)] = fa.product(fb)
      final def flatMap[A, B](fa: Decoder[A])(f: A => Decoder[B]): Decoder[B]           = fa.flatMap(f)

      final def raiseError[A](e: DecodingError): Decoder[A]                                    = Decoder.failed(e)
      final def handleErrorWith[A](fa: Decoder[A])(f: DecodingError => Decoder[A]): Decoder[A] = fa.handleErrorWith(f)

      final def tailRecM[A, B](a: A)(f: A => Decoder[Either[A, B]]): Decoder[B] = new Decoder[B] {
        @tailrec
        private[this] def step(c: Cursor, a1: A): Result[B] = f(a1)(c) match {
          case l @ Left(_)     => l.asInstanceOf[Result[B]]
          case Right(Left(a2)) => step(c, a2)
          case Right(Right(b)) => Right(b)
        }

        final def apply(c: Cursor): Result[B] = step(c, a)
      }
    }
}

trait PrimitiveDecoderInstances {
  implicit final val graphDecodeString: Decoder[String] = Decoder.instance { c =>
    c.narrow.focus match {
      case Some(lit: Literal) if lit.isString => Right(lit.lexicalForm)
      case _                                  => Left(DecodingError("Unable to decode node as a literal String", c.history))
    }
  }

  implicit final val graphDecodeBoolean: Decoder[Boolean] = Decoder.instance { c =>
    c.narrow.focus match {
      case Some(lit: Literal) if lit.isBoolean =>
        lit.lexicalForm.toBooleanOption
          .toRight(DecodingError("Unable to decode node as a literal Boolean", c.history))
      case _ => Left(DecodingError("Unable to decode node as a literal Boolean", c.history))
    }
  }

  implicit final val graphDecodeByte: Decoder[Byte]     = numeric(_.toByteOption)
  implicit final val graphDecodeShort: Decoder[Short]   = numeric(_.toShortOption)
  implicit final val graphDecodeInt: Decoder[Int]       = numeric(_.toIntOption)
  implicit final val graphDecodeLong: Decoder[Long]     = numeric(_.toLongOption)
  implicit final val graphDecodeFloat: Decoder[Float]   = numeric(_.toFloatOption)
  implicit final val graphDecodeDouble: Decoder[Double] = numeric(_.toDoubleOption)

  private def numeric[A](f: String => Option[A])(implicit A: ClassTag[A]): Decoder[A] = Decoder.instance { c =>
    c.narrow.focus match {
      case Some(lit: Literal) if lit.isNumeric =>
        f(lit.lexicalForm) match {
          case Some(a) => Right(a)
          case None =>
            Left(
              DecodingError(s"Unable to decode node as a literal ${A.runtimeClass.getSimpleName}", c.history)
            )
        }
      case _ =>
        Left(DecodingError(s"Unable to decode node as a literal ${A.runtimeClass.getSimpleName}", c.history))
    }
  }
}

trait StandardDecoderInstances { this: PrimitiveDecoderInstances =>
  import alleycats.std.set._

  implicit final val graphDecodeUUID: Decoder[UUID] = Decoder.instance { c =>
    c.narrow.focus match {
      case Some(lit: Literal) if lit.isString =>
        Try(UUID.fromString(lit.lexicalForm)).toEither.left
          .map(_ => DecodingError("Unable to decode node as an UUID", c.history))
      case _ => Left(DecodingError("Unable to decode node as an UUID", c.history))
    }
  }

  implicit final val graphDecodeDuration: Decoder[Duration] =
    graphDecodeString.emap { str =>
      Try(Duration(str)).toEither.leftMap(_ => "Unable to decode node as a Duration")
    }

  implicit final val graphDecodeFiniteDuration: Decoder[FiniteDuration] =
    graphDecodeDuration.emap {
      case _: Duration.Infinite     => Left("Unable to decode node as a FiniteDuration")
      case duration: FiniteDuration => Right(duration)
    }

  implicit final val graphDecodeInstant: Decoder[Instant] =
    graphDecodeString.emap { str =>
      Try(Instant.parse(str)).toEither.leftMap(_ => "Unable to decode node as an Instant")
    }

  implicit final val graphDecodePeriod: Decoder[Period] =
    graphDecodeString.emap { str =>
      Try(Period.parse(str)).toEither.leftMap(_ => "Unable to decode node as a Period")
    }

  implicit final def graphDecodeSet[A](implicit A: Decoder[A]): Decoder[Set[A]] = Decoder.instance { c =>
    c.cursors match {
      case Some(cs) => cs.traverse(A.apply)
      case None     => A(c).map(a => Set(a))
    }
  }

  final def graphDecodeSeqFromList[C[_], A](f: Factory[A, C[A]])(implicit A: Decoder[A]): Decoder[C[A]] = {
    import scala.collection.mutable
    @tailrec
    def inner(c: Cursor, acc: Either[DecodingError, mutable.Builder[A, C[A]]]): Either[DecodingError, C[A]] =
      acc match {
        case l @ Left(_)                                                  => l.asInstanceOf[Either[DecodingError, C[A]]]
        case Right(builder) if c.narrow.as[AbsoluteIri] == Right(rdf.nil) => Right(builder.result())
        case Right(builder) =>
          val first = c.down(rdf.first).as[A]
          val rest  = c.down(rdf.rest)
          inner(rest, first.map(a => builder.addOne(a)))
      }

    Decoder.instance { c =>
      inner(c, Right(f.newBuilder))
    }
  }

  implicit final def graphDecodeVector[A](implicit A: Decoder[A]): Decoder[Vector[A]] =
    graphDecodeSeqFromList(Vector)

  implicit final def graphDecodeList[A](implicit A: Decoder[A]): Decoder[List[A]] =
    graphDecodeSeqFromList(List)

  implicit final def graphDecodeArray[A: ClassTag](implicit A: Decoder[A]): Decoder[Array[A]] =
    graphDecodeSeqFromList(Array)

  implicit final def graphDecodeOption[A](implicit A: Decoder[A]): Decoder[Option[A]] = Decoder.instance { c =>
    c.narrow.focus match {
      case Some(_) => A(c).map(Some.apply)
      case None    => Right(None)
    }
  }

  implicit final def graphDecodeSome[A](implicit A: Decoder[A]): Decoder[Some[A]] =
    A.map(a => Some(a))

  implicit final val graphDecodeNone: Decoder[None.type] = Decoder.instance { c =>
    c.narrow.focus match {
      case None    => Right(None)
      case Some(_) => Left(DecodingError("Unable to decode as None, cursor selection matches an element", c.history))
    }
  }

  implicit final def graphDecodeEither[A, B](implicit A: Decoder[A], B: Decoder[B]): Decoder[Either[A, B]] =
    Decoder.instance { c =>
      A(c) match {
        case Left(_)  => B(c).map(b => Right(b))
        case Right(a) => Right(Left(a))
      }
    }
}

trait RdfDecoderInstances {
  implicit final val graphDecodeAbsoluteIri: Decoder[AbsoluteIri] = Decoder.instance { c =>
    c.narrow.focus match {
      case Some(IriNode(iri)) => Right(iri)
      case Some(Literal(lf, _, _)) =>
        Iri.absolute(lf).leftMap(_ => DecodingError("Unable to decode node as an AbsoluteIri", c.history))
      case _ => Left(DecodingError("Unable to decode node as an AbsoluteIri", c.history))
    }
  }

  implicit final val graphDecodeUrl: Decoder[Url] = Decoder.instance { c =>
    c.narrow.focus match {
      case Some(IriNode(iri)) =>
        iri.asUrl match {
          case Some(url) => Right(url)
          case None      => Left(DecodingError("Unable to decode node as an Url", c.history))
        }
      case Some(Literal(lf, _, _)) =>
        Iri.url(lf).leftMap(_ => DecodingError("Unable to decode node as an Url", c.history))
      case _ => Left(DecodingError("Unable to decode node as an Url", c.history))
    }
  }

  implicit final val graphDecodeUrn: Decoder[Urn] = Decoder.instance { c =>
    c.narrow.focus match {
      case Some(IriNode(iri)) =>
        iri.asUrn match {
          case Some(urn) => Right(urn)
          case None      => Left(DecodingError("Unable to decode node as an Urn", c.history))
        }
      case Some(Literal(lf, _, _)) =>
        Iri.urn(lf).leftMap(_ => DecodingError("Unable to decode node as an Urn", c.history))
      case _ => Left(DecodingError("Unable to decode node as an Urn", c.history))
    }
  }

  implicit final val graphDecodeIriNode: Decoder[IriNode] = Decoder.instance { c =>
    c.narrow.focus match {
      case Some(n: IriNode) => Right(n)
      case _                => Left(DecodingError("Unable to decode node as an IriNode", c.history))
    }
  }

  implicit final val graphDecodeIriOrBNode: Decoder[IriOrBNode] = Decoder.instance { c =>
    c.narrow.focus match {
      case Some(n: IriOrBNode) => Right(n)
      case _                   => Left(DecodingError("Unable to decode node as an IriOrBNode", c.history))
    }
  }

  implicit final val graphDecodeBNode: Decoder[BNode] = Decoder.instance { c =>
    c.narrow.focus match {
      case Some(n: BNode) => Right(n)
      case _              => Left(DecodingError("Unable to decode node as an BNode", c.history))
    }
  }

  implicit final val graphDecodeLiteral: Decoder[Literal] = Decoder.instance { c =>
    c.narrow.focus match {
      case Some(n: Literal) => Right(n)
      case _                => Left(DecodingError("Unable to decode node as a Literal", c.history))
    }
  }

  implicit final val graphDecodeNode: Decoder[Node] = Decoder.instance { c =>
    c.narrow.focus match {
      case Some(n: Node) => Right(n)
      case _             => Left(DecodingError("Unable to decode node as a Node", c.history))
    }
  }
}
