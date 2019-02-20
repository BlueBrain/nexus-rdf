package ch.epfl.bluebrain.nexus.rdf.encoder

import ch.epfl.bluebrain.nexus.rdf.Node.IriOrBNode

/**
  * Type class that defines the primary node on a Graph for a generic type A
  *
  * @tparam A the type from where the primary node will be obtained
  */
trait PrimaryNode[A] {
  def apply(value: A): IriOrBNode
}

object PrimaryNode {

  /**
    * Constructs a [[PrimaryNode]] form a function ''A'' => IriOrBNode
    *
    * @param f the mapping function
    */
  final def apply[A](f: A => IriOrBNode): PrimaryNode[A] = f(_)
}
