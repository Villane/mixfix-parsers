package mixfix

import scala.util.parsing.combinator._
import syntactical._
import lexical._
import token._

trait MixFixParsers extends StdTokenParsers with Operators with PackratParsers {
  def expr: Parser[ASTExpr]
  lazy val wrapExpr = expr ^^ { case x => OpHole(x) }

//  def printNamePart(namePart: NamePart) = namePart match {
//    case ExactName(n) => n
//    case ExactKeyword(n) => n
//    case _ => namePart.toString
//  }
//  private val HOLE = "_"
//  def printOpInternal(op: Operator) = op.nameParts map printNamePart mkString HOLE
//  def printOperator(op: Operator) = op.fix match {
//    case Closed => printOpInternal(op)
//    case Prefix => printOpInternal(op) + HOLE
//    case Postfix => HOLE + printOpInternal(op)
//    case Infix(_) => HOLE + printOpInternal(op) + HOLE
//  }
//
//  def expectedGroups(groups: List[OpGroup]) = Some("Expected one of: " + (groups map (_.name) mkString ", "))
//  def expectedOps(ops: List[Operator]) = Some("Expected one of: " + (ops map printOperator mkString ", "))

  def ⋁[T](parsers: List[Parser[T]]/*, errorMsg: Option[String] = None*/): Parser[T] = parsers match {
    case Nil =>
      failure("No parsers!")
    case p :: Nil =>
      p
    case p0 :: p1 :: rest =>
      var res = p0 | p1
      for (p <- rest) res |= p
//      if (errorMsg.isDefined) {
//        new Parser[T]{ def apply(in: Input) = res(in) match {
//          case s: Success[_] => s
//          case ns: NoSuccess => Failure(errorMsg.get, in)
//        }}
//      } else res
      res
  }

  def parseOpGroups(groups: List[OpGroup]/*, errorMsg: Option[String] = None*/): Parser[OpTree] =
    ⋁(groups map { _.parser })

  def parseOpList(opList: OpList): Parser[OpTree] = new OpListParser(opList).parser

  // TODO generalize for any arity
  def parseOpInternal(op: Operator): Parser[Operator ~ (String ~ List[OpTree ~ String])] =
    op.nameParts.size match {
      case 1 => success(op) ~ (op.nameParts(0).parser ~ success(List()))
      case 2 => success(op) ~ (op.nameParts(0).parser ~ repN(1, wrapExpr ~ op.nameParts(1).parser))
      case _ => error("Arity > 2 not implemented")
    }

  class OpListParser(opGroup: OpList) {
    lazy val closedOps = opGroup.operators filter (_.fix == Closed)
    lazy val nonOps = opGroup.operators filter (_.fix == Infix(Non))
    lazy val leftOps = opGroup.operators filter (_.fix == Infix(Left))
    lazy val rightOps = opGroup.operators filter (_.fix == Infix(Right))
    lazy val postOps = opGroup.operators filter (_.fix == Postfix)
    lazy val preOps = opGroup.operators filter (_.fix == Prefix)

//    lazy val expectedClosed = expectedOps(closedOps)
//    lazy val expectedNon = expectedOps(nonOps)
//    lazy val expectedRight = expectedOps(rightOps ::: preOps)
//    lazy val expectedLeft = expectedOps(leftOps ::: postOps)
//    lazy val expectedTighter = expectedGroups(precedenceGraph.higherPrecedence(opGroup))

    private def ifNonEmpty(ops: List[Operator], p: Parser[OpTree]) =
      if (ops.isEmpty) failure("No operators!") else p

    private def names(src: String ~ List[OpTree ~ String]) = src match {
      case n0 ~ ns => n0 :: (ns map (_._2))
    }
    private def holes(src: String ~ List[OpTree ~ String]) = src match {
      case n0 ~ ns => ns map (_._1)
    }

    lazy val parser: PackratParser[OpTree] = closed | left | right | non

    lazy val tighter: PackratParser[OpTree] = parseOpGroups(opGroup.tighterGroups)

    lazy val closed: Parser[OpApply] = ⋁(closedOps map (_.parser)) ^^ {
      case op ~ internal => OpApply(op, names(internal), holes(internal))
    }
    
    lazy val non: PackratParser[OpTree] = ifNonEmpty(nonOps,
      tighter ~ ⋁(nonOps map (_.parser)) ~ tighter ^^ {
        case el ~ (op ~ internal) ~ er => OpApply(op, names(internal), el :: holes(internal) ::: List(er))
      })

    lazy val right: PackratParser[OpTree] = ifNonEmpty(rightOps ::: preOps,
      (⋁(preOps map (_.parser)) ^^ {
        case op ~ internal => OpApply(op, names(internal), holes(internal)) 
      } |
      tighter ~ ⋁(rightOps map (_.parser)) ^^ {
        case el ~ (op ~ internal) => OpApply(op, names(internal), el :: holes(internal)) 
      }
      ) ~ (right | tighter) ^^ {
        case part ~ r => OpApply(part.op, part.names, part.holes ::: List(r))
      })

    lazy val left: PackratParser[OpTree] = ifNonEmpty(leftOps ::: postOps,
      (left | tighter) ~
      (⋁(postOps map (_.parser)) ^^ {
        case op ~ internal => OpApply(op, names(internal), holes(internal)) 
      } |
      ⋁(leftOps map (_.parser)) ~ tighter ^^ {
        case (op ~ internal) ~ er => OpApply(op, names(internal), holes(internal) ::: List(er)) 
      }
      ) ^^ {
        case l ~ part => OpApply(part.op, part.names, l :: part.holes)
      })

//    lazy val leftAssoc: PackratParser[OpTree] = ifNonEmpty(leftOps ::: postOps,
//      tighter ~ (leftRest +) ^^ {
//        case el ~ ops =>
//          (el /: ops) {
//            (left, partOp) => OpApply(partOp.op, partOp.names, left :: partOp.holes.toList)
//          }
//      })
//
//    lazy val leftRest: PackratParser[OpApply] =
//      ⋁(postOps map { _.parser ^^ {
//        case op ~ internal => OpApply(op, names(internal), holes(internal))
//      }}) |
//      ⋁(leftOps map { _.parser ~ tighter ^^ {
//          case op ~ internal ~ er => OpApply(op, names(internal), holes(internal) ::: List(er))
//      }})
//
//    lazy val rightAssoc: PackratParser[OpTree] = ifNonEmpty(rightOps ::: preOps,
//      (rightRest +) ~ tighter ^^ {
//        case ops ~ er =>
//          (ops :\ er) {
//            (partOp, right) => OpApply(partOp.op, partOp.names, partOp.holes ::: List(right))
//          }
//      })
//  
//    lazy val rightRest: PackratParser[OpApply] =
//      ⋁(preOps map { _.parser ^^ {
//          case op ~ internal => OpApply(op, names(internal), holes(internal))
//        }
//      }) |
//      ⋁(rightOps map { o =>
//        tighter ~ o.parser ^^ {
//          case el ~ (op ~ internal) => OpApply(op, names(internal), el :: holes(internal))
//        }
//      })
  }

}