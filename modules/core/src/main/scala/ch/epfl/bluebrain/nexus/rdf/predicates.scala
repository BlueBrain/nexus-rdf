package ch.epfl.bluebrain.nexus.rdf

import fastparse.all._

//noinspection TypeAnnotation
private[rdf] object predicates {
  val lowerAlpha = CharIn('a' to 'z')
  val upperAlpha = CharIn('A' to 'Z')
  val alpha      = lowerAlpha | upperAlpha
  val digit      = CharIn('0' to '9')

  val `scheme` = alpha ~ (alpha | digit | CharIn("+-.")).rep
}
