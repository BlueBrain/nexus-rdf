package ch.epfl.bluebrain.nexus.rdf.bench

import ch.epfl.bluebrain.nexus.rdf.GraphConfiguration
import ch.epfl.bluebrain.nexus.rdf.syntax.circe._
import org.openjdk.jmh.annotations.{Benchmark, Scope, State}

//noinspection TypeAnnotation
/**
  * Benchmark on Graph operations
  * To run it, execute on the sbt shell: ''jmh:run -i 20 -wi 10 -f1 -t1 .*JsonAsGraph.*''
  * Which means "10 iterations" "10 warmup iterations" "1 fork" "1 thread"
  * Benchmark                       Mode    Cnt     Score     Error   Units
  * JsonAsGraph.asGraphWithCast     thrpt   10      69,760 ±  1,869   ops/s
  * JsonAsGraph.asGraphWithoutCast  thrpt   10      111,786 ± 4,803   ops/s
  */
@State(Scope.Thread)
class JsonAsGraph {

  val json  = jsonContentOf("/schema.json")

  @Benchmark
  def asGraphWithCast(): Unit = {
    implicit val config = GraphConfiguration(castDateTypes = true)
    val _               = json.asGraph.right.get
  }

  @Benchmark
  def asGraphWithoutCast(): Unit = {
    implicit val config = GraphConfiguration(castDateTypes = false)
    val _               = json.asGraph.right.get
  }
}
