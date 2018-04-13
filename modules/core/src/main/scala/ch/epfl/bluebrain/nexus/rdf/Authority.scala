package ch.epfl.bluebrain.nexus.rdf

/**
  * The Authority part of an IRI as defined by RFC 3987.
  *
  * @param userInfo the optional user info part
  * @param host     the host part
  * @param port     the optional port part
  */
final case class Authority(userInfo: Option[UserInfo], host: Host, port: Option[Port])