package ch.epfl.bluebrain.nexus.rdf

import java.util.UUID

import cats.implicits._
import ch.epfl.bluebrain.nexus.rdf.Decoder.Result
import ch.epfl.bluebrain.nexus.rdf.Iri.AbsoluteIri
import ch.epfl.bluebrain.nexus.rdf.Node.{IriNode, Literal}

import scala.reflect.ClassTag
import scala.util.Try

trait Decoder[A] extends Serializable { self =>

  def apply(cursor: Cursor): Result[A]

  final def map[B](f: A => B): Decoder[B] = new Decoder[B] {
    final def apply(cursor: Cursor): Result[B] =
      self(cursor).map(f)
  }

  final def flatMap[B](f: A => Decoder[B]): Decoder[B] = new Decoder[B] {
    final def apply(cursor: Cursor): Result[B] = self(cursor) match {
      case Right(a)    => f(a)(cursor)
      case l @ Left(_) => l.asInstanceOf[Result[B]]
    }
  }

  final def or[AA >: A](d: => Decoder[AA]): Decoder[AA] = new Decoder[AA] {
    final def apply(cursor: Cursor): Result[AA] =
      self(cursor) match {
        case Left(_)      => d(cursor)
        case r @ Right(_) => r
      }
  }

  final def emap[B](f: A => Either[String, B]): Decoder[B] = new Decoder[B] {
    final def apply(c: Cursor): Decoder.Result[B] =
      self(c) match {
        case Right(a) =>
          f(a) match {
            case r @ Right(_)  => r.asInstanceOf[Result[B]]
            case Left(message) => Left(DecodingFailure(message, c.history))
          }
        case l @ Left(_) => l.asInstanceOf[Result[B]]
      }
  }

}

object Decoder extends PrimitiveDecoderInstances with StandardDecoderInstances with RdfDecoderInstances {

  final type Result[A] = Either[DecodingFailure, A]

  @inline
  final def apply[A](implicit instance: Decoder[A]): Decoder[A] = instance

  final def instance[A](f: Cursor => Result[A]): Decoder[A] = new Decoder[A] {
    final def apply(c: Cursor): Result[A] = f(c)
  }

  implicit final val graphDecodeCursor: Decoder[Cursor] = instance(Right(_))
}

trait PrimitiveDecoderInstances {
  implicit final val graphDecodeString: Decoder[String] = Decoder.instance { c =>
    c.focus match {
      case Some(lit: Literal) if lit.isString => Right(lit.lexicalForm)
      case _                                  => Left(DecodingFailure("Unable to decode node as a literal String", c.history))
    }
  }

  implicit final val graphDecodeBoolean: Decoder[Boolean] = Decoder.instance { c =>
    c.focus match {
      case Some(lit: Literal) if lit.isBoolean =>
        lit.lexicalForm.toBooleanOption
          .toRight(DecodingFailure("Unable to decode node as a literal Boolean", c.history))
      case _ => Left(DecodingFailure("Unable to decode node as a literal Boolean", c.history))
    }
  }

  implicit final val graphDecodeInt: Decoder[Int]       = numeric(_.toIntOption)
  implicit final val graphDecodeLong: Decoder[Long]     = numeric(_.toLongOption)
  implicit final val graphDecodeFloat: Decoder[Float]   = numeric(_.toFloatOption)
  implicit final val graphDecodeDouble: Decoder[Double] = numeric(_.toDoubleOption)

  private def numeric[A](f: String => Option[A])(implicit A: ClassTag[A]): Decoder[A] = Decoder.instance { c =>
    c.focus match {
      case Some(lit: Literal) if lit.isNumeric =>
        f(lit.lexicalForm) match {
          case Some(a) => Right(a)
          case None =>
            Left(
              DecodingFailure(s"Unable to decode node as a literal ${A.runtimeClass.getSimpleName}", c.history)
            )
        }
      case _ =>
        Left(DecodingFailure(s"Unable to decode node as a literal ${A.runtimeClass.getSimpleName}", c.history))
    }
  }
}

trait StandardDecoderInstances {
  implicit final val graphDecodeUUID: Decoder[UUID] = Decoder.instance { c =>
    c.focus match {
      case Some(lit: Literal) if lit.isString =>
        Try(UUID.fromString(lit.lexicalForm)).toEither.left
          .map(_ => DecodingFailure("Unable to decode node as an UUID", c.history))
      case _ => Left(DecodingFailure("Unable to decode node as an UUID", c.history))
    }
  }

  implicit final def graphDecodeSet[A](implicit A: Decoder[A]): Decoder[Set[A]] = Decoder.instance { c =>
    c.downSet.foldLeft(Right(Set.empty): Result[Set[A]]) {
      case (l @ Left(_), _) => l
      case (Right(set), el) => A(el).map(set + _)
    }
  }

  implicit final def graphDecodeList[A](implicit A: Decoder[A]): Decoder[List[A]] = Decoder.instance { c =>
    c.downList.traverse(A.apply)
  }

  implicit final def graphDecodeOption[A](implicit A: Decoder[A]): Decoder[Option[A]] = Decoder.instance { c =>
    c.focus match {
      case Some(_) => A(c).map(Some.apply)
      case None    => Right(None)
    }
  }
}

trait RdfDecoderInstances {
  implicit final val graphDecodeAbsoluteIri: Decoder[AbsoluteIri] = Decoder.instance { c =>
    c.focus match {
      case Some(IriNode(iri)) => Right(iri)
      case _                  => Left(DecodingFailure("Unable to decode node as an AbsoluteIri", c.history))
    }
  }
}
