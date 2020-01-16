package ch.epfl.bluebrain.nexus.rdf.derivation.decoder

import java.util.UUID

import ch.epfl.bluebrain.nexus.rdf.derivation.Fixture.View.{AggregateElasticSearchView, ElasticSearchView, ViewRef}
import ch.epfl.bluebrain.nexus.rdf.derivation.Fixture.{mapping, View}
import ch.epfl.bluebrain.nexus.rdf.syntax.all._
import ch.epfl.bluebrain.nexus.rdf.{Decoder, RdfSpec}

class MagnoliaDecoderSpec extends RdfSpec {

  "A MagnoliaDecoder" should {
    "derive a Decoder for fixed ElasticSearchView" in {
      val model = toJenaModel(jsonWithContext("/elasticsearch-view.json"))
      val graph = fromJenaModel(url"http://example.com/id", model)

      val expected = ElasticSearchView(
        id = url"http://example.com/id",
        uuid = Some(UUID.fromString("3aa14a1a-81e7-4147-8306-136d8270bb01")),
        notAMapping = mapping,
        resourceSchemas = Set(nxv"Schema", nxv"Resource"),
        resourceTypes = Set(nxv"MyType", nxv"MyType2"),
        resourceTag = Some("one"),
        sourceAsText = Some(false)
      )

      Decoder[View].apply(graph.cursor) shouldEqual Right(expected)
    }
    "derive a Decoder for fixed AggregateElasticSearchView" in {
      val model = toJenaModel(jsonWithContext("/aggregate-elasticsearch-view.json"))
      val graph = fromJenaModel(url"http://example.com/id", model)

      val expected = AggregateElasticSearchView(
        id = url"http://example.com/id",
        uuid = Some(UUID.fromString("3aa14a1a-81e7-4147-8306-136d8270bb01")),
        views = List(
          ViewRef("account1/project1", url"http://example.com/view1"),
          ViewRef("account1/project2", url"http://example.com/view2"),
          ViewRef("account1/project3", url"http://example.com/view3"),
          ViewRef("account1/project4", url"http://example.com/view4")
        )
      )

      Decoder[View].apply(graph.cursor) shouldEqual Right(expected)
    }
  }
}
