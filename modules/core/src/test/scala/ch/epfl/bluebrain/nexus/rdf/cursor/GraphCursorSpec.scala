package ch.epfl.bluebrain.nexus.rdf.cursor

import java.util.UUID

import ch.epfl.bluebrain.nexus.rdf.Graph._
import ch.epfl.bluebrain.nexus.rdf.Iri.AbsoluteIri
import ch.epfl.bluebrain.nexus.rdf.Node.{literal, IriNode, IriOrBNode}
import ch.epfl.bluebrain.nexus.rdf.cursor.CursorOp._
import ch.epfl.bluebrain.nexus.rdf.cursor.GraphCursor.{FailedCursor, TopCursor}
import ch.epfl.bluebrain.nexus.rdf.cursor.GraphCursorSpec._
import ch.epfl.bluebrain.nexus.rdf.encoder.NodeEncoderError.{IllegalConversion, IllegalType, NoElementToEncode}
import ch.epfl.bluebrain.nexus.rdf.syntax.node._
import ch.epfl.bluebrain.nexus.rdf.syntax.node.encoder._
import ch.epfl.bluebrain.nexus.rdf.syntax.node.unsafe._
import ch.epfl.bluebrain.nexus.rdf.{Graph, Node}
import org.scalatest.{EitherValues, Matchers, OptionValues, WordSpecLike}

class GraphCursorSpec extends WordSpecLike with Matchers with OptionValues with EitherValues {
  "A GraphCursor" should {

    /**
      * The graph encoding for the following JSON-LD
      * {
      *   "@context": {
      *     "name": "http://schema.org/name",
      *     "description": "http://schema.org/description",
      *     "coordinates": "http://schema.org/coordinates",
      *     "image": {
      *       "@id": "http://schema.org/image",
      *       "@type": "@id"
      *     },
      *     "geo": "http://schema.org/geo",
      *     "uuid": "http://schema.org/uuid",
      *     "other": "http://schema.org/other",
      *     "latitude": {
      *       "@id": "http://schema.org/latitude",
      *       "@type": "xsd:float"
      *     },
      *     "longitude": {
      *       "@id": "http://schema.org/longitude",
      *       "@type": "xsd:float"
      *     },
      *     "xsd": "http://www.w3.org/2001/XMLSchema#"
      *   },
      *   "@id": "http://example.com",
      *   "name": "The Empire State Building",
      *   "description": "The Empire State...",
      *   "image": {
      *     "name": "Front",
      *     "description": "Image of..."
      *   },
      *   "other": [ 1.3, 2.4, 3.5],
      *   "uuid" : "b46ff2d0-f9d1-48e4-94eb-65d1a756c607",
      *   "geo": [{
      *     "coordinates": {
      *       "latitude": "10.75",
      *       "longitude": "10.98"
      *     }
      *     },{
      *     "coordinates": {
      *       "latitude": "40.75",
      *       "longitude": "73.98"
      *     }
      *   }]
      * }
      */
    val id       = url"http://example.com"
    val imageId  = b"imageid"
    val geoId1   = b"geoId1"
    val geoId2   = b"geoId2"
    val coordId1 = b"coordinatesId1"
    val coordId2 = b"coordinatesId2"

    val graph = Graph(
      (id, schema.desc, "The Empire State..."),
      (id, schema.name, "The Empire State Building"),
      (id, schema.img, imageId),
      (imageId, schema.desc, "Image of..."),
      (imageId, schema.name, "Front"),
      (id, schema.uuid, "b46ff2d0-f9d1-48e4-94eb-65d1a756c607"),
      (id, schema.other, literal(1.3)),
      (id, schema.other, literal(2.4)),
      (id, schema.other, literal(3.5)),
      (id, schema.geo, geoId1),
      (id, schema.geo, geoId2),
      (geoId1, schema.coord, coordId1),
      (coordId1, schema.lat, literal(10.75f)),
      (coordId1, schema.lng, literal(10.98f)),
      (geoId2, schema.coord, coordId2),
      (coordId2, schema.lat, literal(40.75f)),
      (coordId2, schema.lng, literal(73.98f))
    )

    val c = GraphCursor(id, graph)

    "obtain the cursor from the graph primary node" in {
      c.focus.value shouldEqual graph.cursor(id).focus.value
      c.history shouldEqual graph.cursor(id).history
      c.values shouldEqual graph.cursor(id).values
    }

    "navigate down a simple" in {
      c.downField(schema.desc).focus.value shouldEqual ("The Empire State...": Node)
      c.downField(schema.desc).succeeded shouldEqual true
      c.downField(schema.desc).values.value shouldEqual List[Node]("The Empire State...")
    }

    "fail to navigate to a non existing property" in {
      failedChecks(c.downField(schema.lat))
    }

    "navigate down an object" in {
      c.downField(schema.img).downField(schema.desc).focus.value shouldEqual ("Image of...": Node)
      c.downField(schema.img).downField(schema.name).focus.value shouldEqual ("Front": Node)
    }

    "navigate down an array element" in {
      c.downField(schema.geo).downAt(geoId1).values.value shouldEqual List(geoId1)
      c.downField(schema.geo).downAt(geoId1).focus.value shouldEqual geoId1
      c.downField(schema.geo).downAt(geoId1).downField(schema.coord).focus.value shouldEqual coordId1
      c.downField(schema.geo).downAt(geoId2).downField(schema.coord).focus.value shouldEqual coordId2

      c.downField(schema.geo)
        .downAt(geoId1)
        .downField(schema.coord)
        .downField(schema.lat)
        .focus
        .value shouldEqual literal(10.75f)

      c.downField(schema.geo).values.value shouldEqual Set(geoId1, geoId2)
    }

    "navigate down the whole array" in {
      val result = c
        .downField(schema.geo)
        .downArray
        .map(_.downField(schema.coord).downField(schema.lat).focus.as[Float].right.value)
      result shouldEqual Set(10.75f, 40.75f)
    }

    "fetch encoded values" in {
      c.downField(schema.geo)
        .downAt(geoId1)
        .downField(schema.coord)
        .downField(schema.lat)
        .focus
        .as[Float]
        .right
        .value shouldEqual 10.75f

      c.focus.as[AbsoluteIri].right.value shouldEqual id.value

      c.downField(schema.desc).focus.as[String].right.value shouldEqual "The Empire State..."
      c.downField(schema.other).values.asListOf[Double].right.value should contain theSameElementsAs List(1.3, 2.4, 3.5)
      c.downField(schema.uuid).focus.as[UUID].right.value shouldEqual UUID.fromString(
        "b46ff2d0-f9d1-48e4-94eb-65d1a756c607")
    }

    "fail to fetch encoded values" in {
      c.downField(schema.geo)
        .downAt(geoId1)
        .downField(schema.coord)
        .downField(schema.lat)
        .focus
        .as[Int]
        .left
        .value shouldBe a[IllegalConversion]

      c.downField(schema.geo).downField(schema.coord).focus.as[String].left.value shouldEqual NoElementToEncode
      c.downField(schema.other).values.asListOf[String].left.value shouldBe a[IllegalConversion]
      c.downField(schema.other).values.asListOf[AbsoluteIri].left.value shouldBe a[IllegalType]
      c.downField(schema.desc).focus.as[UUID].left.value shouldBe a[IllegalConversion]

    }

    "fail to navigate to a down property when the array element hasn't been selected" in {
      failedChecks(c.downField(schema.geo).downField(schema.coord))
    }

    "return None when getting the focus for an array element hasn't been selected" in {
      c.downField(schema.geo).focus shouldEqual None
    }

    "fail to navigate through siblings in an array" in {
      failedChecks(c.downField(schema.geo).downAt(geoId1).field(schema.geo))
    }

    "fail to navigate down a non existing array element" in {
      failedChecks(c.downField(schema.geo).downAt(coordId1))
    }

    "fail to navigate down a cursor which already failed" in {
      failedChecks(c.downField(schema.geo).downAt(coordId1).downField(schema.lat))
    }

    "return empty down array when the selection does not exists" in {
      c.downField(schema.geo).downAt(coordId1).downArray shouldEqual Set.empty
    }

    "return empty down array when the element is not an array" in {
      c.downField(schema.img).downArray shouldEqual Set.empty
    }

    "navigate to siblings" in {
      c.downField(schema.img).downField(schema.desc).field(schema.name).focus.value shouldEqual ("Front": Node)
      c.downField(schema.img).field(schema.name).focus.value shouldEqual ("The Empire State Building": Node)
      c.downField(schema.name)
        .field(schema.img)
        .downField(schema.desc)
        .up
        .downField(schema.name)
        .field(schema.desc)
        .focus
        .value shouldEqual ("Image of...": Node)

      c.downField(schema.geo).field(schema.img).downField(schema.name).focus.value shouldEqual ("Front": Node)

      c.downField(schema.geo)
        .downAt(geoId1)
        .downField(schema.coord)
        .downField(schema.lat)
        .field(schema.lng)
        .focus
        .value shouldEqual literal(10.98f)
    }

    "navigate up" in {
      c.downField(schema.img).downField(schema.desc).up.focus.value shouldEqual imageId
      c.downField(schema.img).downField(schema.desc).up.up.focus.value shouldEqual id
      c.downField(schema.img).downField(schema.desc).up.up shouldBe a[TopCursor]
      c.downField(schema.geo).downAt(geoId2).downField(schema.coord).up.up.up.focus.value shouldEqual id
    }

    "navigate to the top" in {
      c.downField(schema.img).downField(schema.desc).top.focus.value shouldEqual id
      c.downField(schema.geo).downAt(geoId2).downField(schema.coord).top.focus.value shouldEqual id
    }

    "history explore" in {
      val image: IriOrBNode => Boolean       = schema.img
      val description: IriOrBNode => Boolean = schema.desc
      val name: IriOrBNode => Boolean        = schema.name

      val expected1 = List(DownField(image), DownField(description), MoveUp, DownField(name))
      val expected2 = List(DownField(image), DownField(description), MoveUp, DownField(name), MoveTop)

      c.downField(image).downField(description).up.downField(name).history shouldEqual expected1
      c.downField(image).downField(description).up.downField(name).top.history shouldEqual expected2
    }
    def failedChecks(failedCursor: GraphCursor) = {
      failedCursor shouldBe a[FailedCursor]
      failedCursor.focus shouldEqual None
      failedCursor.failed shouldEqual true
      failedCursor.values shouldEqual None
    }
  }
}
object GraphCursorSpec {
  object schema {
    val desc: IriNode  = url"http://schema.org/description"
    val name: IriNode  = url"http://schema.org/name"
    val geo: IriNode   = url"http://schema.org/geo"
    val other: IriNode = url"http://schema.org/other"
    val uuid: IriNode  = url"http://schema.org/uuid"
    val coord: IriNode = url"http://schema.org/coordinates"
    val lat: IriNode   = url"http://schema.org/latitude"
    val lng: IriNode   = url"http://schema.org/longitude"
    val img: IriNode   = url"http://schema.org/image"
  }
}
