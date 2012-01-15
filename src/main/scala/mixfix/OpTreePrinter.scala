package mixfix

trait OpTreePrinter { self: Operators =>

  def printOpGraph(graph: PrecedenceGraph) = {
    for (group <- graph.nodes) {
      print(group.name + " -> ")
      print(graph.edges(group) map (_.name) mkString ", ")
      println()
    }
  }

  def printOpHole(e: OpTree): String = e match {
    case OpHole(expr) => expr.toString
    case OpApply(Operator(Closed, _), _, _) => printOpTree(e)
    case _ => "(" + printOpTree(e) + ")"
  }

  def printOpHoleNoParens(e: OpTree): String = e match {
    case OpHole(expr) => expr.toString
    case _ => printOpTree(e)
  }

  def printOpTree(e: OpTree): String = e match {
    case OpHole(expr) =>
      expr.toString
    case OpApply(op, names, holes) => op.fix match {
      case Prefix =>
        interleave(names, holes map printOpHole) mkString " " 
      case Infix(_) =>
        interleave(holes map printOpHole, names) mkString " " 
      case Postfix =>
        interleave(holes map printOpHole, names) mkString " "
      case Closed =>
        interleave(names, holes map printOpHoleNoParens) mkString ""
    }
  }

  private def interleave[T](as: Seq[T], bs: Seq[T]): Seq[T] = {
    val ai = as.iterator
    val bi = bs.iterator
    val res = collection.mutable.ArrayBuffer[T]()
    while (ai.hasNext && bi.hasNext) {
      res += ai.next
      res += bi.next
    }
    while (ai.hasNext) res += ai.next
    while (bi.hasNext) res += bi.next
    res.toSeq
  }

}