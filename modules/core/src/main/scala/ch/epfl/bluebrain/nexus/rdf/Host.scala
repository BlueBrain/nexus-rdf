package ch.epfl.bluebrain.nexus.rdf

import cats.syntax.either._
import cats.{Eq, Show}
import ch.epfl.bluebrain.nexus.rdf.predicates._
import fastparse.all._

import scala.collection.{immutable, SeqView}

/**
  * Host part of an Iri as defined in RFC 3987.
  */
sealed abstract class Host extends Product with Serializable {

  /**
    * @return true if the host is an IPv4 address, false otherwise
    */
  def isIPv4: Boolean = false

  /**
    * @return true if the host is an IPv6 address, false otherwise
    */
  def isIPv6: Boolean = false

  /**
    * @return true if the host is a named host, false otherwise
    */
  def isNamed: Boolean = false

  /**
    * @return Some(this) if this is an IPv4Host, None otherwise
    */
  def asIPv4: Option[IPv4Host] = None

  /**
    * @return Some(this) if this is an IPv6Host, None otherwise
    */
  def asIPv6: Option[IPv6Host] = None

  /**
    * @return Some(this) if this is a NamedHost, None otherwise
    */
  def asNamed: Option[NamedHost] = None

  /**
    * @return a string representation of this host, as specified by RFC 3987.
    */
  def asString: String
}

object Host {

  /**
    * Constructs a new IPv4Host from the argument bytes.
    *
    * @param byte1 the first byte of the address
    * @param byte2 the second byte of the address
    * @param byte3 the third byte of the address
    * @param byte4 the fourth byte of the address
    * @return the IPv4Host represented by these bytes
    */
  final def ipv4(byte1: Byte, byte2: Byte, byte3: Byte, byte4: Byte): IPv4Host =
    IPv4Host(byte1, byte2, byte3, byte4)

  /**
    * Attempt to construct a new IPv4Host from its 32bit representation.
    *
    * @param bytes a 32bit IPv4 address
    * @return Right(IPv4Host(bytes)) if the bytes is a 4byte array, Left(error) otherwise
    */
  final def ipv4(bytes: Array[Byte]): Either[String, IPv4Host] =
    IPv4Host(bytes)

  /**
    * Attempt to construct a new IPv4Host from its string representation as specified by RFC 3987.
    *
    * @param string the string to parse as an IPv4 address.
    * @return Right(IPv4Host(bytes)) if the string conforms to specification, Left(error) otherwise
    */
  final def ipv4(string: String): Either[String, IPv4Host] =
    IPv4Host(string)

  /**
    * Attempt to construct a new IPv6Host from its 128bit representation.
    *
    * @param bytes a 128bit IPv6 address
    * @return Right(IPv6Host(bytes)) if the bytes is a 16byte array, Left(error) otherwise
    */
  def ipv6(bytes: Array[Byte]): Either[String, IPv6Host] =
    IPv6Host(bytes)
}

/**
  * An IPv4 host representation as specified by RFC 3987.
  *
  * @param bytes the underlying bytes
  */
final case class IPv4Host private[rdf] (bytes: immutable.Seq[Byte]) extends Host {
  override def isIPv4: Boolean          = true
  override def asIPv4: Option[IPv4Host] = Some(this)
  override lazy val asString: String    = bytes.map(_ & 0xFF).mkString(".")
}

object IPv4Host {
  private val parser = (Start ~ `IPv4address` ~ End).map {
    case (b1, b2, b3, b4) => apply(b1, b2, b3, b4)
  }

  /**
    * Attempt to construct a new IPv4Host from its 32bit representation.
    *
    * @param bytes a 32bit IPv4 address
    * @return Right(IPv4Host(bytes)) if the bytes is a 4byte array, Left(error) otherwise
    */
  final def apply(bytes: Array[Byte]): Either[String, IPv4Host] =
    Either
      .catchNonFatal(fromBytes(bytes))
      .leftMap(_ => "Illegal IPv4Host byte representation")

  /**
    * Attempt to construct a new IPv4Host from its string representation as specified by RFC 3987.
    *
    * @param string the string to parse as an IPv4 address.
    * @return Right(IPv4Host(bytes)) if the string conforms to specification, Left(error) otherwise
    */
  final def apply(string: String): Either[String, IPv4Host] =
    parser.parse(string) match {
      case Parsed.Success(host, _)   => Right(host)
      case Parsed.Failure(_, idx, _) => Left(s"Failed to parse string '$string' as IPv4Host at index '$idx'")
    }

  /**
    * Constructs a new IPv4Host from the argument bytes.
    *
    * @param byte1 the first byte of the address
    * @param byte2 the second byte of the address
    * @param byte3 the third byte of the address
    * @param byte4 the fourth byte of the address
    * @return the IPv4Host represented by these bytes
    */
  final def apply(byte1: Byte, byte2: Byte, byte3: Byte, byte4: Byte): IPv4Host =
    fromBytes(Array(byte1, byte2, byte3, byte4))

  private def fromBytes(bytes: Array[Byte]): IPv4Host = {
    require(bytes.length == 4)
    new IPv4Host(immutable.Seq(bytes: _*))
  }

  final implicit val ipv4HostShow: Show[IPv4Host] =
    Show.show(_.asString)

  final implicit val ipv4HostEq: Eq[IPv4Host] =
    Eq.fromUniversalEquals
}

final case class IPv6Host private[rdf] (bytes: immutable.Seq[Byte]) extends Host {
  override def isIPv6: Boolean          = true
  override def asIPv6: Option[IPv6Host] = Some(this)

  override lazy val asString: String =
    asString(bytes.view)

  lazy val asMixedString: String =
    asString(bytes.view(0, 12)) + ":" + bytes.view(12, 16).map(_ & 0xFF).mkString(".")

  private def asString(bytes: SeqView[Byte, immutable.Seq[Byte]]): String =
    bytes.grouped(2).map(two => Integer.toHexString(BigInt(two.toArray).intValue())).mkString(":")
}

object IPv6Host {

  /**
    * Attempt to construct a new IPv6Host from its 128bit representation.
    *
    * @param bytes a 128bit IPv6 address
    * @return Right(IPv6Host(bytes)) if the bytes is a 16byte array, Left(error) otherwise
    */
  final def apply(bytes: Array[Byte]): Either[String, IPv6Host] =
    Either
      .catchNonFatal(fromBytes(bytes))
      .leftMap(_ => "Illegal IPv6Host byte representation")

  private def fromBytes(bytes: Array[Byte]): IPv6Host = {
    require(bytes.length == 16)
    new IPv6Host(immutable.Seq(bytes: _*))
  }

  final implicit val ipv6HostShow: Show[IPv6Host] =
    Show.show(_.asString)

  final implicit val ipv6HostEq: Eq[IPv6Host] =
    Eq.fromUniversalEquals
}

trait NamedHost extends Host
final case object NamedHost extends Host {
  override def asString: String = ""
}
