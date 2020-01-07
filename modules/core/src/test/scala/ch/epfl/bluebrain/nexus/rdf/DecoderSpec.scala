package ch.epfl.bluebrain.nexus.rdf

import java.util.UUID
import java.util.concurrent.TimeUnit

import ch.epfl.bluebrain.nexus.rdf.Iri.{AbsoluteIri, Url, Urn}
import ch.epfl.bluebrain.nexus.rdf.Node.{BNode, IriNode, IriOrBNode, Literal}
import ch.epfl.bluebrain.nexus.rdf.implicits._

import scala.concurrent.duration.{Duration, FiniteDuration}

class DecoderSpec extends RdfSpec {

  private val id    = url"http://example.com/id"
  private val model = toJenaModel(jsonWithContext("/decoder.json"))
  private val graph = fromJenaModel(id, model)
  private val c     = graph.cursor

  "A Decoder" should {
    "successfully decode primitive types" in {
      c.down(nxv"boolean").as[Boolean].rightValue shouldEqual true
      c.down(nxv"int").as[Int].rightValue shouldEqual 1
      c.down(nxv"long").as[Long].rightValue shouldEqual 1L
      c.down(nxv"float").as[Float].rightValue shouldEqual 1.2f
      c.down(nxv"double").as[Double].rightValue shouldEqual 1.2d
      c.down(nxv"string").as[String].rightValue shouldEqual "some string"
      c.down(nxv"string").as[Set[String]].rightValue shouldEqual Set("some string")
    }
    "successfully decode standard types" in {
      c.down(nxv"uuid").as[UUID].rightValue.toString shouldEqual "3aa14a1a-81e7-4147-8306-136d8270bb01"
      c.down(nxv"uuid").as[Option[UUID]].rightValue.value.toString shouldEqual "3aa14a1a-81e7-4147-8306-136d8270bb01"
      c.down(nxv"unknownPredicate").as[Option[UUID]].rightValue shouldEqual None
      c.down(nxv"list").as[List[Int]].rightValue shouldEqual List(1, 2, 2)
      c.down(nxv"list").as[Vector[Int]].rightValue shouldEqual Vector(1, 2, 2)
      c.downSet(nxv"set").as[Set[Int]].rightValue should contain theSameElementsAs Set(1, 2)
      c.down(nxv"duration").as[Duration].rightValue shouldEqual Duration.Inf
      c.down(nxv"finiteDuration").as[FiniteDuration].rightValue shouldEqual FiniteDuration(3, TimeUnit.MINUTES)
    }
    "successfully decode rdf types" in {
      c.as[AbsoluteIri].rightValue shouldEqual id
      c.down(nxv"url").as[AbsoluteIri].rightValue shouldEqual id
      c.down(nxv"idUrl").as[AbsoluteIri].rightValue shouldEqual id
      c.down(nxv"url").as[Url].rightValue shouldEqual id
      c.down(nxv"idUrl").as[Url].rightValue shouldEqual id
      c.down(nxv"urn").as[Urn].rightValue shouldEqual urn"urn:uuid:3aa14a1a-81e7-4147-8306-136d8270bb01"
      c.down(nxv"idUrn").as[Urn].rightValue shouldEqual urn"urn:uuid:3aa14a1a-81e7-4147-8306-136d8270bb01"
      c.as[Cursor].rightValue shouldEqual c
      c.as[Node].rightValue shouldEqual IriNode(id)
      c.as[IriNode].rightValue shouldEqual IriNode(id)
      c.as[IriOrBNode].rightValue shouldEqual IriNode(id)
      c.down(nxv"string").as[Literal].rightValue.lexicalForm shouldEqual "some string"
      c.down(nxv"bnode").as[BNode].rightValue
    }
    "fail to decode" in {
      c.as[Int].leftValue
      c.as[Float].leftValue
      c.as[Double].leftValue
      c.as[Long].leftValue
      c.as[UUID].leftValue
      c.as[List[Int]].leftValue
      c.as[List[String]].leftValue
      c.as[Set[Int]].leftValue
      c.as[Set[String]].leftValue
      c.down(nxv"float").as[Int].leftValue
      c.down(nxv"float").as[String].leftValue
      c.down(nxv"float").as[AbsoluteIri].leftValue
      c.down(nxv"float").as[Url].leftValue
      c.down(nxv"float").as[Urn].leftValue
      c.down(nxv"float").as[List[Float]].leftValue
      c.down(nxv"float").as[Vector[Float]].leftValue
      c.down(nxv"string").as[UUID].leftValue
      c.down(nxv"duration").as[FiniteDuration].leftValue
    }
  }

}
