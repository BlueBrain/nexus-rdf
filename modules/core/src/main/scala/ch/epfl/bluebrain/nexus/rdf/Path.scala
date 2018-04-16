package ch.epfl.bluebrain.nexus.rdf

import cats.syntax.either._
import cats.{Eq, Show}
import org.parboiled2.Parser.DeliveryScheme.Either
import org.parboiled2._

/**
  * Path part of an Iri as defined in RFC 3987.
  */
sealed abstract class Path extends Product with Serializable {

  /**
    * @return true if the path contains no characters, false otherwise
    */
  def isEmpty: Boolean

  /**
    * @return true if this path is a [[ch.epfl.bluebrain.nexus.rdf.Slash]] (ends with a slash '/'), false otherwise
    */
  def isSlash: Boolean

  /**
    * @return true if this path is a [[ch.epfl.bluebrain.nexus.rdf.Segment]] (ends with a segment), false otherwise
    */
  def isSegment: Boolean

  /**
    * @return Some(this) if this path is an Empty path, None otherwise
    */
  def asEmpty: Option[Empty]

  /**
    * @return Some(this) if this path is a Slash (ends with a slash '/') path, None otherwise
    */
  def asSlash: Option[Slash]

  /**
    * @return Some(this) if this path is a Segment (ends with a segment) path, None otherwise
    */
  def asSegment: Option[Segment]

  /**
    * @return true if this path ends with a slash ('/'), false otherwise
    */
  def endsWithSlash: Boolean = isSlash
}

object Path {

  /**
    * Constant value for a single slash.
    */
  final val / = Slash(Empty)

  /**
    * Attempts to parse the argument string as an `ipath-abempty` Path as defined by RFC 3987.
    *
    * @param string the string to parse as a Path
    * @return Right(Path) if the parse succeeds, Left(error) otherwise
    */
  final def apply(string: String): Either[String, Path] =
    abempty(string)

  /**
    * Attempts to parse the argument string as an `ipath-abempty` Path as defined by RFC 3987.
    *
    * @param string the string to parse as a Path
    * @return Right(Path) if the parse succeeds, Left(error) otherwise
    */
  final def abempty(string: String): Either[String, Path] =
    new IriParser(string).`ipath-abempty`
      .run()
      .leftMap(_.format(string, new ErrorFormatter(showExpected = false, showTraces = false)))

  final implicit val pathShow: Show[Path] = Show.show {
    case Empty                  => ""
    case Slash(rest)            => pathShow.show(rest) + "/"
    case Segment(segment, rest) => pathShow.show(rest) + segment
  }

  final implicit val pathEq: Eq[Path] = Eq.fromUniversalEquals
}

/**
  * An empty path.
  */
sealed trait Empty extends Path

/**
  * An empty path.
  */
final case object Empty extends Empty {
  def isEmpty: Boolean           = true
  def isSlash: Boolean           = false
  def isSegment: Boolean         = false
  def asEmpty: Option[Empty]     = Some(this)
  def asSlash: Option[Slash]     = None
  def asSegment: Option[Segment] = None
}

/**
  * A path that ends with a '/' character.
  *
  * @param rest the remainder of the path (excluding the '/')
  */
final case class Slash(rest: Path) extends Path {
  def isEmpty: Boolean           = false
  def isSlash: Boolean           = true
  def isSegment: Boolean         = false
  def asEmpty: Option[Empty]     = None
  def asSlash: Option[Slash]     = Some(this)
  def asSegment: Option[Segment] = None
}

/**
  * A path that ends with a segment.
  *
  * @param rest the remainder of the path (excluding the segment denoted by this)
  */
final case class Segment private[rdf] (segment: String, rest: Path) extends Path {
  def isEmpty: Boolean           = false
  def isSlash: Boolean           = false
  def isSegment: Boolean         = true
  def asEmpty: Option[Empty]     = None
  def asSlash: Option[Slash]     = None
  def asSegment: Option[Segment] = Some(this)
}
