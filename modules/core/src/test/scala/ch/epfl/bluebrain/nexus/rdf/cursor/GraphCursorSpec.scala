package ch.epfl.bluebrain.nexus.rdf.cursor

import ch.epfl.bluebrain.nexus.rdf.Graph._
import ch.epfl.bluebrain.nexus.rdf.Node.{literal, IriNode, IriOrBNode}
import ch.epfl.bluebrain.nexus.rdf.cursor.CursorOp._
import ch.epfl.bluebrain.nexus.rdf.cursor.GraphCursor.{FailedCursor, TopCursor}
import ch.epfl.bluebrain.nexus.rdf.cursor.GraphCursorSpec._
import ch.epfl.bluebrain.nexus.rdf.syntax.node._
import ch.epfl.bluebrain.nexus.rdf.syntax.node.unsafe._
import ch.epfl.bluebrain.nexus.rdf.{Graph, Node}
import org.scalatest.{Matchers, OptionValues, WordSpecLike}

class GraphCursorSpec extends WordSpecLike with Matchers with OptionValues {
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
      c.values shouldEqual c.values
    }

    "navigate down a simple" in {
      c.downField(schema.desc).focus.value shouldEqual ("The Empire State...": Node)
      c.downField(schema.desc).succeeded shouldEqual true
    }

    "fail to navigate to a non existing property" in {
      failedChecks(c.downField(schema.lat))
    }

    "navigate down an object" in {
      c.downField(schema.img).downField(schema.desc).focus.value shouldEqual ("Image of...": Node)
      c.downField(schema.img).downField(schema.name).focus.value shouldEqual ("Front": Node)
    }

    "navigate down an array element" in {
      c.downField(schema.geo).downAt(geoId1).values shouldEqual None
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
    val coord: IriNode = url"http://schema.org/coordinates"
    val lat: IriNode   = url"http://schema.org/latitude"
    val lng: IriNode   = url"http://schema.org/longitude"
    val img: IriNode   = url"http://schema.org/image"
  }
}
