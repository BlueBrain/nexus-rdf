package ch.epfl.bluebrain.nexus.rdf.bench
import java.io.{ByteArrayInputStream, File}

import akka.http.scaladsl.model.Uri
import ch.epfl.bluebrain.nexus.rdf.IriParser
import io.circe.parser._
import org.apache.jena.iri.IRIFactory
import org.apache.jena.rdf.model.ModelFactory
import org.apache.jena.riot.{Lang, RDFDataMgr}
import org.openjdk.jmh.annotations.{Benchmark, Scope, State}

import scala.io.Source

//noinspection TypeAnnotation
@State(Scope.Thread)
class Parsing {

  val iris = {
    import scala.collection.JavaConverters._
    val json = parse(Source.fromFile(new File("/Users/roman/code/work/nexus/nexus-rdf/bench/src/main/resources/schema.json")).mkString).right.get
    val model = ModelFactory.createDefaultModel()
    RDFDataMgr.read(model, new ByteArrayInputStream(json.noSpaces.getBytes), Lang.JSONLD)

    val list = model.listStatements().asScala.foldLeft[List[String]](Nil) {
      case (acc, stmt) =>
        val subj = if (stmt.getSubject.isURIResource) List(stmt.getSubject.getURI) else Nil
        val pred = if (stmt.getPredicate.isURIResource) List(stmt.getPredicate.getURI) else Nil
        val obj = if (stmt.getObject.isURIResource) List(stmt.getObject.asResource().getURI) else Nil
      subj ++ pred ++ obj ++ acc
    }
    println(s"IRIs: ${list.size}")
    list
  }

  val iriFactory = IRIFactory.iriImplementation()

  @Benchmark
  def parseIri(): Unit = {
    iris.foreach(i => new IriParser(i).parseAbsolute)
  }

  @Benchmark
  def parseAkkaUri(): Unit = {
    iris.foreach(Uri.apply)
  }

  @Benchmark
  def parseJenaIri(): Unit = {
    iris.foreach(i => {
      val iri = iriFactory.create(i)
      val _ = iri.violations(true)
    })
  }
}
