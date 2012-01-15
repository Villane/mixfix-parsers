package mixfix

import scala.util.parsing.combinator._
import syntactical._
import lexical._
import token._

trait Operators { self: StdTokenParsers =>
  import lexical.{Identifier}

  type ASTExpr

  /**
   * A tree that captures expressions, of which there are two types:
   * OpApply - Operator Application
   * OpHole  - All other expressions, can appear in "holes" in the operator syntax
   * 
   * OpHole wraps an ASTExpr from the host grammar
   */
  sealed trait OpTree
  case class OpApply(op: Operator, names: List[String], holes: List[OpTree]) extends OpTree
  case class OpHole(expr: ASTExpr) extends OpTree

  /** Part of an operator name. Name parts must have holes between them. */
  trait NamePart {
    def matches(parsed: String): Boolean 
    def parser: Parser[String]
  }

  case class ExactName(name: String) extends NamePart {
    def matches(parsed: String) = parsed == name
    def parser: Parser[String] = elem("opnamepart", { x: Elem => x match {
      case Identifier(parsed) if matches(parsed) => true
      case _ => false
    }}) ^^ { case x => x.chars }
  }

  case class ExactKeyword(name: String) extends NamePart {
    def matches(parsed: String) = parsed == name
    def parser: Parser[String] = name
  }

  class ExceptNames(names: () => collection.Set[String]) extends NamePart {
    private lazy val _names = names()
    def matches(parsed: String) = !(_names contains parsed)
    def parser: Parser[String] = elem("otheropnamepart", { x: Elem => x match {
      case Identifier(parsed) if matches(parsed) => true
      case _ => false
    }}) ^^ { case x => x.chars }
  }

  case class Operator(fix: Fixity, nameParts: List[NamePart]) {
    lazy val parser = parseOpInternal(this)
  }

  trait OpGroup {
    def name: String
    def parser: Parser[OpTree]
  }

  case class OpList(name: String, operators: List[Operator]) extends OpGroup {
    var graph: PrecedenceGraph = null
    lazy val parser = parseOpList(this)

    def tighterGroups = graph.higherPrecedence(this)
  }

  trait TightPseudoGroup extends OpGroup

  def parseOpInternal(op: Operator): Parser[Operator ~ (String ~ List[OpTree ~ String])]
  def parseOpList(opList: OpList): Parser[OpTree]

  class PrecedenceGraph(val nodes: List[OpGroup], definedEdges: Map[OpGroup, List[OpGroup]]) {
    val edges = {
      var es = definedEdges
      for (node <- nodes) {
        node match {
          case opList: OpList =>
            opList.graph = this
          case _ =>
        }
        if (!(es contains node))
          es += (node -> Nil)
      }
      // add catch-all edges to all nodes
      for (node <- nodes if node.isInstanceOf[TightPseudoGroup]) {
        es = es.map(pair => if (node eq pair._1) pair else pair._1 -> (pair._2 ::: List(node)))
      }
      es
    }

    def higherPrecedence(group: OpGroup) = edges(group)
    lazy val allGroups = nodes
    lazy val allOperators = nodes collect { case OpList(name, ops) => ops } flatten
    lazy val concreteGroups = allGroups filter { _.isInstanceOf[OpList] }
    lazy val concreteOperators = allOperators filter { op =>
      op.nameParts forall { n => n.isInstanceOf[ExactName] || n.isInstanceOf[ExactKeyword] }
    }
    lazy val concreteNames = concreteOperators flatMap (_.nameParts) collect { nm => nm match {
      case ExactName(name) => name
      case ExactKeyword(name) => name
    }}
  }

}