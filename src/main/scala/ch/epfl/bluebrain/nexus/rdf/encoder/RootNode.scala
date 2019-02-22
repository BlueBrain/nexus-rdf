package ch.epfl.bluebrain.nexus.rdf.encoder

import ch.epfl.bluebrain.nexus.rdf.Node.IriOrBNode

/**
  * Type class that defines the root node on a Graph for a generic type A
  *
  * @tparam A the type from where the root node will be obtained
  */
trait RootNode[A] {
  def apply(value: A): IriOrBNode
}

object RootNode {

  /**
    * Constructs a [[RootNode]] form a function ''A'' => IriOrBNode
    *
    * @param f the mapping function
    */
  final def apply[A](f: A => IriOrBNode): RootNode[A] = f(_)
}
