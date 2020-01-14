package ch.epfl.bluebrain.nexus.rdf

import java.time.{Instant, Period}
import java.util.UUID
import java.util.concurrent.TimeUnit

import cats.Contravariant
import cats.implicits._
import ch.epfl.bluebrain.nexus.rdf.EncoderSpec.IntValue
import ch.epfl.bluebrain.nexus.rdf.Graph.Triple
import ch.epfl.bluebrain.nexus.rdf.Iri.{AbsoluteIri, Url}
import ch.epfl.bluebrain.nexus.rdf.Node.{IriNode, Literal}
import ch.epfl.bluebrain.nexus.rdf.Vocabulary.{rdf, schema}
import ch.epfl.bluebrain.nexus.rdf.syntax.all._

import scala.concurrent.duration.{Duration, FiniteDuration}

class EncoderSpec extends RdfSpec {

  private val example    = "http://example.com/"
  private val exampleIri = Url.unsafe(example)

  "An Encoder" should {
    "contramap" in {
      val encoder: Encoder[Int] = Contravariant[Encoder].contramap(Encoder[String])(_.toString)
      val g                     = encoder(1)
      g.node shouldEqual Literal("1")
      g.triples.isEmpty shouldEqual true
    }
    "correctly encode primitives" in {
      Encoder[Byte].apply(1.toByte) shouldEqual Graph(1.toByte)
      Encoder[Short].apply(1.toShort) shouldEqual Graph(1.toShort)
      Encoder[Int].apply(1) shouldEqual Graph(1)
      Encoder[Long].apply(1L) shouldEqual Graph(1L)
      Encoder[Double].apply(1.0) shouldEqual Graph(1.0)
      Encoder[Float].apply(1.0f) shouldEqual Graph(1.0f)
      Encoder[Boolean].apply(true) shouldEqual Graph(true)
      Encoder[String].apply("asd") shouldEqual Graph("asd")
    }
    "correctly encode UUIDs" in {
      val uuid = UUID.randomUUID()
      Encoder[UUID].apply(uuid) shouldEqual Graph(uuid.toString)
    }
    "correctly encode FiniteDuration" in {
      val duration = FiniteDuration(3, TimeUnit.DAYS)
      Encoder[FiniteDuration].apply(duration) shouldEqual Graph(duration.toString())
    }
    "correctly encode Duration" in {
      val duration = Duration.Inf
      Encoder[Duration].apply(duration) shouldEqual Graph(duration.toString())
    }
    "correctly encode Instant" in {
      val instant = Instant.now()
      Encoder[Instant].apply(instant) shouldEqual Graph(instant.toString())
    }
    "correctly encode Period" in {
      val period = Period.ofDays(3)
      Encoder[Period].apply(period) shouldEqual Graph(period.toString())
    }
    "correctly encode AbsoluteIris" in {
      Encoder[AbsoluteIri].apply(schema.value) shouldEqual Graph(schema.value)
    }

    "correctly encode Sets" when {
      "the values are objects" in {
        val set = Set(
          IntValue(url"${example}1", 1),
          IntValue(url"${example}2", 2),
          IntValue(url"${example}3", 3)
        )
        val expected = Set[Triple](
          (url"${example}1", schema.value, 1),
          (url"${example}2", schema.value, 2),
          (url"${example}3", schema.value, 3)
        )
        val g = Encoder[Set[IntValue]].apply(set)
        g.triples shouldEqual expected
      }
      "the values are primitives" in {
        val set = Set(1, 2, 3)
        val g   = Encoder[Set[Int]].apply(set)
        // a set graph of primitives has no triples
        g.triples.isEmpty shouldEqual true
        // one of the nodes is selected as an anchor
        set.map[Node](Literal.apply).contains(g.node) shouldEqual true
        // prepending a (IriOrBNode, IriNode) pair will generate triples using the values in the set graph in object position
        val withPrependedId = (exampleIri: IriNode, schema.value: IriNode) :: g
        withPrependedId shouldEqual Graph(
          exampleIri,
          Set(
            (exampleIri, schema.value, 1),
            (exampleIri, schema.value, 2),
            (exampleIri, schema.value, 3)
          )
        )
      }
      "the set contains a single primitive" in {
        val set = Set(1)
        val g   = Encoder[Set[Int]].apply(set)
        // a set graph of primitives has no triples
        g.triples.isEmpty shouldEqual true
        g.node shouldEqual Literal(1)
      }
      "the set contains a single object" in {
        val set      = Set(IntValue(url"${example}1", 1))
        val expected = Set[Triple]((url"${example}1", schema.value, 1))
        val g        = Encoder[Set[IntValue]].apply(set)
        g.triples shouldEqual expected
        g.node shouldEqual (url"${example}1": IriNode)
      }
      "the set is empty" in {
        Encoder[Set[IntValue]].apply(Set.empty).triples.isEmpty shouldEqual true
        Encoder[Set[IntValue]].apply(Set.empty).node.isBlank shouldEqual true
        Encoder[Set[Int]].apply(Set.empty).node.isBlank shouldEqual true
        Encoder[Set[Int]].apply(Set.empty).triples.isEmpty shouldEqual true
      }
    }

    "correctly encode a Some" in {
      val g = Encoder[Option[IntValue]].apply(Some(IntValue(exampleIri, 1)))
      g.node shouldEqual (exampleIri: IriNode)
      g.triples shouldEqual Set[Triple]((exampleIri, schema.value, 1))
    }
    "correctly encode a None" in {
      val g = Encoder[Option[IntValue]].apply(None)
      g.triples.isEmpty shouldEqual true
      g.node.isBlank shouldEqual true
    }
    "correctly encode Either" in {
      Encoder[Either[Int, Boolean]].apply(Left(1)).node shouldEqual Literal(1)
      Encoder[Either[Int, Boolean]].apply(Right(true)).node shouldEqual Literal(true)
    }

    "correctly encode sequences of objects" when {
      val seq: Seq[IntValue] = List(
        IntValue(url"${example}1", 1),
        IntValue(url"${example}2", 2),
        IntValue(url"${example}3", 3)
      )
      val seqExpected = toJenaModel(
        Graph(
          b"1",
          Set(
            (b"1", rdf.first, url"${example}1"),
            (b"1", rdf.rest, b"2"),
            (b"2", rdf.first, url"${example}2"),
            (b"2", rdf.rest, b"3"),
            (b"3", rdf.first, url"${example}3"),
            (b"3", rdf.rest, rdf.nil),
            (url"${example}1", schema.value, 1),
            (url"${example}2", schema.value, 2),
            (url"${example}3", schema.value, 3)
          )
        )
      )
      val oneSeq: Seq[IntValue] = List(
        IntValue(url"${example}1", 1)
      )
      val oneSeqExpected = toJenaModel(
        Graph(
          b"1",
          Set(
            (b"1", rdf.first, url"${example}1"),
            (b"1", rdf.rest, rdf.nil),
            (url"${example}1", schema.value, 1)
          )
        )
      )

      "collection type is a Seq" in {
        val g = toJenaModel(Encoder[Seq[IntValue]].apply(seq))
        g isIsomorphicWith seqExpected shouldEqual true
      }
      "collection type is a Seq with one element" in {
        val g = toJenaModel(Encoder[Seq[IntValue]].apply(oneSeq))
        g isIsomorphicWith oneSeqExpected shouldEqual true
      }
      "collection type is a Seq with no elements" in {
        val g = Encoder[Seq[IntValue]].apply(Seq.empty)
        g shouldEqual Graph(rdf.nil, Set.empty)
      }
      "collection type is a List" in {
        val g = toJenaModel(Encoder[List[IntValue]].apply(seq.toList))
        g isIsomorphicWith seqExpected shouldEqual true
      }
      "collection type is a List with one element" in {
        val g = toJenaModel(Encoder[List[IntValue]].apply(oneSeq.toList))
        g isIsomorphicWith oneSeqExpected shouldEqual true
      }
      "collection type is a List with no elements" in {
        val g = Encoder[List[IntValue]].apply(Nil)
        g shouldEqual Graph(rdf.nil, Set.empty)
      }
      "collection type is a Vector" in {
        val g = toJenaModel(Encoder[Vector[IntValue]].apply(seq.toVector))
        g isIsomorphicWith seqExpected shouldEqual true
      }
      "collection type is a Vector with one element" in {
        val g = toJenaModel(Encoder[Vector[IntValue]].apply(oneSeq.toVector))
        g isIsomorphicWith oneSeqExpected shouldEqual true
      }
      "collection type is a Vector with no elements" in {
        val g = Encoder[Vector[IntValue]].apply(Vector.empty)
        g shouldEqual Graph(rdf.nil, Set.empty)
      }
      "collection type is an Array" in {
        val g = toJenaModel(Encoder[Array[IntValue]].apply(seq.toArray))
        g isIsomorphicWith seqExpected shouldEqual true
      }
      "collection type is an Array with one element" in {
        val g = toJenaModel(Encoder[Array[IntValue]].apply(oneSeq.toArray))
        g isIsomorphicWith oneSeqExpected shouldEqual true
      }
      "collection type is a Array with no elements" in {
        val g = Encoder[Array[IntValue]].apply(Array.empty)
        g shouldEqual Graph(rdf.nil, Set.empty)
      }

      "collection has a Foldable" in {
        val encoder: Encoder[List[IntValue]] = Encoder.encodeFoldable[List, IntValue]
        val g                                = toJenaModel(encoder(seq.toList))
        g isIsomorphicWith seqExpected shouldEqual true
      }
    }

    "correctly encode sequences of primitives" when {
      val seq: Seq[Int] = List(1, 2, 3)
      val seqExpected = toJenaModel(
        Graph(
          b"1",
          Set[Triple](
            (b"1", rdf.first, 1),
            (b"1", rdf.rest, b"2"),
            (b"2", rdf.first, 2),
            (b"2", rdf.rest, b"3"),
            (b"3", rdf.first, 3),
            (b"3", rdf.rest, rdf.nil)
          )
        )
      )
      val oneSeq: Seq[Int] = List(1)
      val oneSeqExpected = toJenaModel(
        Graph(
          b"1",
          Set(
            (b"1", rdf.first, 1),
            (b"1", rdf.rest, rdf.nil)
          )
        )
      )

      "collection type is a Seq" in {
        val g = toJenaModel(Encoder[Seq[Int]].apply(seq))
        g isIsomorphicWith seqExpected shouldEqual true
      }
      "collection type is a Seq with one element" in {
        val g = toJenaModel(Encoder[Seq[Int]].apply(oneSeq))
        g isIsomorphicWith oneSeqExpected shouldEqual true
      }
      "collection type is a Seq with no elements" in {
        val g = Encoder[Seq[Int]].apply(Seq.empty)
        g shouldEqual Graph(rdf.nil, Set.empty)
      }
      "collection type is a List" in {
        val g = toJenaModel(Encoder[List[Int]].apply(seq.toList))
        g isIsomorphicWith seqExpected shouldEqual true
      }
      "collection type is a List with one element" in {
        val g = toJenaModel(Encoder[List[Int]].apply(oneSeq.toList))
        g isIsomorphicWith oneSeqExpected shouldEqual true
      }
      "collection type is a List with no elements" in {
        val g = Encoder[List[Int]].apply(Nil)
        g shouldEqual Graph(rdf.nil, Set.empty)
      }
      "collection type is a Vector" in {
        val g = toJenaModel(Encoder[Vector[Int]].apply(seq.toVector))
        g isIsomorphicWith seqExpected shouldEqual true
      }
      "collection type is a Vector with one element" in {
        val g = toJenaModel(Encoder[Vector[Int]].apply(oneSeq.toVector))
        g isIsomorphicWith oneSeqExpected shouldEqual true
      }
      "collection type is a Vector with no elements" in {
        val g = Encoder[Vector[Int]].apply(Vector.empty)
        g shouldEqual Graph(rdf.nil, Set.empty)
      }
      "collection type is an Array" in {
        val g = toJenaModel(Encoder[Array[Int]].apply(seq.toArray))
        g isIsomorphicWith seqExpected shouldEqual true
      }
      "collection type is an Array with one element" in {
        val g = toJenaModel(Encoder[Array[Int]].apply(oneSeq.toArray))
        g isIsomorphicWith oneSeqExpected shouldEqual true
      }
      "collection type is a Array with no elements" in {
        val g = Encoder[Array[Int]].apply(Array.empty)
        g shouldEqual Graph(rdf.nil, Set.empty)
      }

      "collection has a Foldable" in {
        val encoder: Encoder[List[Int]] = Encoder.encodeFoldable[List, Int]
        val g                           = toJenaModel(encoder(seq.toList))
        g isIsomorphicWith seqExpected shouldEqual true
      }
    }
  }
}

object EncoderSpec {
  final case class IntValue(id: AbsoluteIri, value: Int)
  object IntValue {
    implicit val intValueEncoder: Encoder[IntValue] = Encoder.instance {
      case IntValue(id, value) => Graph(id, Set((id, schema.value, value)))
    }
  }
}
