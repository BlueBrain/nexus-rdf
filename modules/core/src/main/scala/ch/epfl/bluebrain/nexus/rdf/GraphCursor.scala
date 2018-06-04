package ch.epfl.bluebrain.nexus.rdf

import ch.epfl.bluebrain.nexus.rdf.Node.{IriNode, IriOrBNode}

sealed trait GraphCursor extends Product with Serializable {

  /**
    * @return the cursor current values
    */
  def values: Set[Node]

  /**
    * @return the cursor current values which are of type [[IriOrBNode]]
    */
  def ids: Set[IriOrBNode] = values.collect { case n: IriOrBNode => n }

  /**
    * @param p the triple predicate used to test matches
    * @param o the triple objects used to test matches
    * @return the cursor from the available ids with the provided predicate and object functions
    */
  def down(p: IriNode => Boolean = _ => true, o: Node => Boolean = _ => true): GraphCursor
}

object GraphCursor {

  /**
    * An cursor which does not contain values
    */
  final case object EmptyCursor extends GraphCursor {
    override def values: Set[Node]                                                                    = Set.empty
    override def down(p: IriNode => Boolean = _ => true, o: Node => Boolean = _ => true): GraphCursor = this
  }

  sealed trait NonEmpty extends GraphCursor {
    private[rdf] val g: Graph

    def down(p: IriNode => Boolean = _ => true, o: Node => Boolean = _ => true): GraphCursor =
      GraphCursor(ids.flatMap(s => g.objects(_ == s, p)), g)
  }

  /**
    * A cursor that contains a single value
    *
    * @param value the cursor current value
    * @param g     the underlying graph
    */
  final case class SingleCursor(value: Node, private[rdf] val g: Graph) extends NonEmpty {
    override def values: Set[Node] = Set(value)
  }

  /**
    * A cursor that contains multiple values
    *
    * @param values the cursor current values
    * @param g      the underlying graph
    */
  final case class MultiCursor(values: Set[Node], private[rdf] val g: Graph) extends NonEmpty

  /**
    * @param values the values to build the cursor
    * @param g
    * @return a new [[GraphCursor]] with the provided values
    */
  final def apply(values: Set[Node], g: Graph): GraphCursor = values.toList match {
    case Nil         => EmptyCursor
    case head :: Nil => SingleCursor(head, g)
    case _           => MultiCursor(values, g)
  }

}
