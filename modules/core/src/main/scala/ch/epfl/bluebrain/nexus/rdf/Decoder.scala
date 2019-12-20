package ch.epfl.bluebrain.nexus.rdf

import java.util.UUID

import cats.implicits._
import ch.epfl.bluebrain.nexus.rdf.Decoder.Result
import ch.epfl.bluebrain.nexus.rdf.Iri.{AbsoluteIri, Url, Urn}
import ch.epfl.bluebrain.nexus.rdf.Node.{BNode, IriNode, IriOrBNode, Literal}
import ch.epfl.bluebrain.nexus.rdf.Vocabulary.rdf

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
    c.narrow.focus match {
      case Some(lit: Literal) if lit.isString => Right(lit.lexicalForm)
      case _                                  => Left(DecodingFailure("Unable to decode node as a literal String", c.history))
    }
  }

  implicit final val graphDecodeBoolean: Decoder[Boolean] = Decoder.instance { c =>
    c.narrow.focus match {
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
    c.narrow.focus match {
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
  import alleycats.std.set._

  implicit final val graphDecodeUUID: Decoder[UUID] = Decoder.instance { c =>
    c.narrow.focus match {
      case Some(lit: Literal) if lit.isString =>
        Try(UUID.fromString(lit.lexicalForm)).toEither.left
          .map(_ => DecodingFailure("Unable to decode node as an UUID", c.history))
      case _ => Left(DecodingFailure("Unable to decode node as an UUID", c.history))
    }
  }

  implicit final def graphDecodeSet[A](implicit A: Decoder[A]): Decoder[Set[A]] = Decoder.instance { c =>
    c.cursors match {
      case Some(cs) => cs.traverse(A.apply)
      case None     => A(c).map(a => Set(a))
    }
  }

  implicit final def graphDecodeVector[A](implicit A: Decoder[A]): Decoder[Vector[A]] = Decoder.instance { c =>
    // TODO: make this stack safe
    c.narrow.focus match {
      case Some(IriNode(rdf.nil)) => Right(Vector.empty)
      case Some(_: IriOrBNode) =>
        for {
          head <- A(c.down(rdf.first))
          tail <- graphDecodeVector[A].apply(c.down(rdf.rest))
        } yield head +: tail
      case Some(_: Literal) =>
        val errMsg =
          "Unable to decode value to Vector, expected either rdf:nil or node with rdf:first and rdf:rest, but found literal."
        Left(DecodingFailure(errMsg, c.history))
      case None =>
        Left(DecodingFailure("Unable to decode Vector, expected cursor to be centered on a single Node", c.history))
    }
  }

  implicit final def graphDecodeList[A](implicit A: Decoder[A]): Decoder[List[A]] = Decoder.instance { c =>
    // TODO: make this stack safe
    c.narrow.focus match {
      case Some(IriNode(rdf.nil)) => Right(Nil)
      case Some(_: IriOrBNode) =>
        for {
          head <- A(c.down(rdf.first))
          tail <- graphDecodeList[A].apply(c.down(rdf.rest))
        } yield head :: tail
      case Some(_: Literal) =>
        val errMsg =
          "Unable to decode value to List, expected either rdf:nil or node with rdf:first and rdf:rest, but found literal."
        Left(DecodingFailure(errMsg, c.history))
      case None =>
        Left(DecodingFailure("Unable to decode List, expected cursor to be centered on a single Node", c.history))
    }
  }

  implicit final def graphDecodeOption[A](implicit A: Decoder[A]): Decoder[Option[A]] = Decoder.instance { c =>
    c.narrow.focus match {
      case Some(_) => A(c).map(Some.apply)
      case None    => Right(None)
    }
  }
}

trait RdfDecoderInstances {
  implicit final val graphDecodeAbsoluteIri: Decoder[AbsoluteIri] = Decoder.instance { c =>
    c.narrow.focus match {
      case Some(IriNode(iri)) => Right(iri)
      case Some(Literal(lf, _, _)) =>
        Iri.absolute(lf).leftMap(_ => DecodingFailure("Unable to decode node as an AbsoluteIri", c.history))
      case _ => Left(DecodingFailure("Unable to decode node as an AbsoluteIri", c.history))
    }
  }

  implicit final val graphDecodeUrl: Decoder[Url] = Decoder.instance { c =>
    c.narrow.focus match {
      case Some(IriNode(iri)) =>
        iri.asUrl match {
          case Some(url) => Right(url)
          case None      => Left(DecodingFailure("Unable to decode node as an Url", c.history))
        }
      case Some(Literal(lf, _, _)) =>
        Iri.url(lf).leftMap(_ => DecodingFailure("Unable to decode node as an Url", c.history))
      case _ => Left(DecodingFailure("Unable to decode node as an Url", c.history))
    }
  }

  implicit final val graphDecodeUrn: Decoder[Urn] = Decoder.instance { c =>
    c.narrow.focus match {
      case Some(IriNode(iri)) =>
        iri.asUrn match {
          case Some(urn) => Right(urn)
          case None      => Left(DecodingFailure("Unable to decode node as an Urn", c.history))
        }
      case Some(Literal(lf, _, _)) =>
        Iri.urn(lf).leftMap(_ => DecodingFailure("Unable to decode node as an Urn", c.history))
      case _ => Left(DecodingFailure("Unable to decode node as an Urn", c.history))
    }
  }

  implicit final val graphDecodeIriNode: Decoder[IriNode] = Decoder.instance { c =>
    c.narrow.focus match {
      case Some(n: IriNode) => Right(n)
      case _                => Left(DecodingFailure("Unable to decode node as an IriNode", c.history))
    }
  }

  implicit final val graphDecodeIriOrBNode: Decoder[IriOrBNode] = Decoder.instance { c =>
    c.narrow.focus match {
      case Some(n: IriOrBNode) => Right(n)
      case _                   => Left(DecodingFailure("Unable to decode node as an IriOrBNode", c.history))
    }
  }

  implicit final val graphDecodeBNode: Decoder[BNode] = Decoder.instance { c =>
    c.narrow.focus match {
      case Some(n: BNode) => Right(n)
      case _              => Left(DecodingFailure("Unable to decode node as an BNode", c.history))
    }
  }

  implicit final val graphDecodeLiteral: Decoder[Literal] = Decoder.instance { c =>
    c.narrow.focus match {
      case Some(n: Literal) => Right(n)
      case _                => Left(DecodingFailure("Unable to decode node as a Literal", c.history))
    }
  }

  implicit final val graphDecodeNode: Decoder[Node] = Decoder.instance { c =>
    c.narrow.focus match {
      case Some(n: Node) => Right(n)
      case _             => Left(DecodingFailure("Unable to decode node as a Node", c.history))
    }
  }
}
