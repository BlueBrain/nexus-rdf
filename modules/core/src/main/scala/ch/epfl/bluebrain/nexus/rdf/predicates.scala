package ch.epfl.bluebrain.nexus.rdf

import fastparse.all._

//noinspection TypeAnnotation
private[rdf] object predicates {
  val lowerAlpha = CharIn('a' to 'z')
  val upperAlpha = CharIn('A' to 'Z')
  val alpha      = lowerAlpha | upperAlpha
  val digit      = CharIn('0' to '9')

  val digit04 = CharIn('0' to '4')
  val digit05 = CharIn('0' to '5')
  val digit19 = CharIn('1' to '9')

  val `dec-octet` = P(
    ("2" ~ ((digit04 ~ digit) | ("5" ~ digit05)))
      | ("1" ~ digit ~ digit)
      | (digit19 ~ digit)
      | digit
  ).!.map(_.toInt.toByte)

  val `IPv4address` = `dec-octet` ~ "." ~ `dec-octet` ~ "." ~ `dec-octet` ~ "." ~ `dec-octet`

  val `scheme` = alpha ~ (alpha | digit | CharIn("+-.")).rep
}
