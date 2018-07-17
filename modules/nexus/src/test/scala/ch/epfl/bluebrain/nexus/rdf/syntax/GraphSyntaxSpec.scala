package ch.epfl.bluebrain.nexus.rdf.syntax

import ch.epfl.bluebrain.nexus.rdf.Graph._
import ch.epfl.bluebrain.nexus.rdf.Iri.AbsoluteIri
import ch.epfl.bluebrain.nexus.rdf.Node
import ch.epfl.bluebrain.nexus.rdf.Node.{BNode, IriNode}
import ch.epfl.bluebrain.nexus.rdf.syntax.GraphSyntaxSpec._
import ch.epfl.bluebrain.nexus.rdf.syntax.circe._
import ch.epfl.bluebrain.nexus.rdf.syntax.nexus._
import ch.epfl.bluebrain.nexus.rdf.syntax.node.encoder._
import ch.epfl.bluebrain.nexus.rdf.syntax.node.unsafe._
import io.circe.Json
import io.circe.parser.parse
import org.scalatest._

import scala.io.Source

class GraphSyntaxSpec extends WordSpecLike with Matchers with TryValues with OptionValues with EitherValues {

  "A GraphSyntax" should {
    val self         = jsonContentOf("/self-reference.json")
    val json         = jsonContentOf("/no_id.json")
    val typedJson    = jsonContentOf("/id_and_types.json")
    val arrayJson    = jsonContentOf("/array.json")
    val arrayOneJson = jsonContentOf("/array-one.json")

    "find @id from a Json-LD without it" in {
      json.asGraph.primaryNode.value shouldBe a[BNode]
      json.asGraph.primaryBNode.value shouldBe a[BNode]
    }

    "find @id from Json-LD with it" in {
      typedJson.asGraph.primaryNode.value shouldEqual url"http://example.org/cars/for-sale#tesla"
      typedJson.asGraph.primaryIriNode.value shouldEqual url"http://example.org/cars/for-sale#tesla"
    }

    "fail to find an @id when it is self-referenced" in {
      self.asGraph.primaryNode shouldEqual None
    }

    "find the @type from the Json-LD without @id" in {
      json.asGraph.primaryTypes shouldEqual Set(url"http://google.com/a")

    }

    "find no types when it is self-referenced" in {
      self.asGraph.primaryTypes shouldEqual Set.empty
    }

    "find the @type from the Json-LD with @id" in {
      typedJson.asGraph.types(url"http://example.org/cars/for-sale#tesla") shouldEqual Set(
        url"http://purl.org/goodrelations/v1#Offering",
        url"http://www.w3.org/2002/07/owl#Ontology")
      typedJson.asGraph.primaryTypes shouldEqual Set(url"http://purl.org/goodrelations/v1#Offering",
                                                     url"http://www.w3.org/2002/07/owl#Ontology")
    }

    "find return no types for id which doesn't have type predicates" in {
      typedJson.asGraph.types(url"http://example.org/cars/for-sale#other") shouldEqual Set.empty
    }

    "navigate to an element" in {
      json.asGraph
        .cursor()
        .downField(url"http://schema.org/image")
        .focus
        .value shouldEqual (url"http://www.civil.usherbrooke.ca/cours/gci215a/empire-state-building.jpg": Node)
    }

    "return a failed cursor when @id is not found" in {
      self.asGraph.cursor().failed shouldEqual true
    }

    "convert back to json" in {
      val other = jsonContentOf("/id_and_type.json")
      other.asGraph.asJson(context(other)).success.value shouldEqual other
    }

    "navigate a graph of array of objects" in {
      val cursor = arrayJson.asGraph.cursor()
      val result = cursor.downField(nxv.identities).downArray.map { cursor =>
        val r = for {
          realm <- cursor.downField(nxv.realm).focus.as[String]
          admin <- cursor.downField(nxv.admin).focus.as[Boolean]
          tpe   <- cursor.downField(rdf.tpe).values.asListOf[AbsoluteIri]
          key = if (tpe.contains(nxv.userRef.value)) nxv.user
          else if (tpe.contains(nxv.groupRef.value)) nxv.group
          else nxv.nonExists
          identity <- cursor.downField(key).focus.as[String]
        } yield (tpe, realm, identity, admin)
        r.right.value
      }
      result shouldEqual Set((List(nxv.userRef.value), "ldap2", "didac", false),
                             (List(nxv.groupRef.value), "ldap", "bbp-ou-neuroinformatics", true))
    }

    "navigate a graph of array of objects with one element" in {
      val c = arrayOneJson.asGraph.cursor()
      val result =
        c.downField(nxv.identities).downArray.map(_.downField(nxv.realm).focus.as[String].right.value)
      result shouldEqual Set("some-realm")
    }

  }

  def context(json: Json): Json = Json.obj("@context" -> json.hcursor.get[Json]("@context").getOrElse(Json.obj()))

  final def jsonContentOf(resourcePath: String): Json =
    parse(Source.fromInputStream(getClass.getResourceAsStream(resourcePath)).mkString).toTry.success.value

}

object GraphSyntaxSpec {
  object nxv {
    val identities: IriNode = url"http://www.example.com/vocab/identities"
    val realm: IriNode      = url"http://www.example.com/vocab/realm"
    val group: IriNode      = url"http://www.example.com/vocab/group"
    val user: IriNode       = url"http://www.example.com/vocab/user"
    val userRef: IriNode    = url"http://www.example.com/vocab/UserRef"
    val admin: IriNode      = url"http://www.example.com/vocab/admin"
    val groupRef: IriNode   = url"http://www.example.com/vocab/GroupRef"
    val nonExists: IriNode  = url"http://www.example.com/vocab/nonExists"
  }
  object rdf {
    val tpe = url"http://www.w3.org/1999/02/22-rdf-syntax-ns#type"

  }
}
