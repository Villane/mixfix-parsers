package mixfix

trait MixFixDSL { self: Operators =>
  def isKeyword(s: String): Boolean 

  implicit def string2namePart(s: String): NamePart =
    if (isKeyword(s))
      ExactKeyword(s)
    else
      ExactName(s)

  def prefix(name: String) = Operator(Prefix, name :: Nil) 
  def postfix(name: String) = Operator(Postfix, name :: Nil)
  def infix(name: String) = Operator(Infix(Left), name :: Nil)
  def infixRight(name: String) = Operator(Infix(Right), name :: Nil)
  def infixNon(name: String) = Operator(Infix(Non), name :: Nil)
  def closed(left: String, right: String) = Operator(Closed, List(left, right))

  def group(name: String, ops: Operator*) = OpList(name, ops.toList)

  def graph(groups: OpGroup*)(precedences: (OpGroup, List[OpGroup])*) = new PrecedenceGraph(
    groups.toList,
    Map() ++ precedences
  )

}