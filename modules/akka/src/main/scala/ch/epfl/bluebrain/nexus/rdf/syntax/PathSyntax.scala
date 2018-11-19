package ch.epfl.bluebrain.nexus.rdf.syntax

import _root_.akka.http.scaladsl.model.Uri
import ch.epfl.bluebrain.nexus.rdf.Iri
import ch.epfl.bluebrain.nexus.rdf.Iri.Path.{Segment, Slash}
import ch.epfl.bluebrain.nexus.rdf.syntax.PathSyntax.{IriPathSyntax, UriPathSyntax}

import scala.annotation.tailrec

trait PathSyntax {

  final implicit def uriPathToIriPath(path: Uri.Path): UriPathSyntax = new UriPathSyntax(path)

  final implicit def iriPathToUriPath(path: Iri.Path): IriPathSyntax = new IriPathSyntax(path)
}

object PathSyntax {

  private[syntax] implicit class UriPathSyntax(private val path: Uri.Path) extends AnyVal {
    @tailrec
    private def inner(acc: Iri.Path, remaining: Uri.Path): Iri.Path = remaining match {
      case Uri.Path.SingleSlash         => Slash(acc)
      case Uri.Path.Empty               => acc
      case Uri.Path.Slash(tail)         => inner(Slash(acc), tail)
      case Uri.Path.Segment(head, tail) => inner(Segment(head, acc), tail)
    }

    /**
      * Convert this  [[Uri.Path]] to [[Iri.Path]]
      */
    def toIriPath: Iri.Path = inner(Iri.Path.Empty, path)
  }

  private[syntax] implicit class IriPathSyntax(private val path: Iri.Path) extends AnyVal {
    @tailrec
    private def inner(acc: Uri.Path, remaining: Iri.Path): Uri.Path = remaining match {
      case Slash(rest)         => inner(acc ++ Uri.Path./, rest)
      case Iri.Path.Empty      => acc
      case Segment(head, tail) => inner(acc + head, tail)
    }

    /**
      * Convert this [[Iri.Path]] to [[Uri.Path]]
      */
    def toUriIriPath: Uri.Path = inner(Uri.Path.Empty, path.reverse)
  }
}
