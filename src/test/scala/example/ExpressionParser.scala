package example

import scala.util.parsing.combinator._
import syntactical._
import lexical._
import token._

import mixfix._

object ExpressionParser extends StdTokenParsers with MixFixParsers with MixFixDSL with OpTreePrinter with PackratParsers {
  type Tokens = StdTokens
  val lexical = new StdLexical
  import lexical.{Keyword, NumericLit, Identifier}

  lexical.delimiters += (
    "(", ")", ":=",
    "^",
    "*", "/",
    "+", "-",
    "<", ">",
    "=", "≠",
    "!",
    "&",
    "|"
  )
  lexical.reserved += (
    "true", "false"
  )

  type ASTExpr = String

  def isKeyword(s: String) = (lexical.delimiters contains s) || (lexical.reserved contains s)

  lazy val statement: PackratParser[String] = expr | declaration
  lazy val declaration: PackratParser[String] = variable ~ ":=" ~ expr ^^ { case nm ~ _ ~ e => nm + " := " + e }
  lazy val expr: PackratParser[String] = opExpr | nonOpExpr
  lazy val nonOpExpr: PackratParser[String] = variable | literal
  lazy val variable: PackratParser[String] = ident
  lazy val literal: PackratParser[String] = numericLit | booleanLit
  lazy val booleanLit: PackratParser[String] = "true" | "false"

  lazy val opExpr = parseOpGroups(precedenceGraph.allGroups) ^^ printOpTree

  object Tightest extends TightPseudoGroup {
    val name = "variable or literal"
    lazy val parser = nonOpExpr ^^ { case x => OpHole(x) }
  }

  lazy val precedenceGraph = {
    val parentheses = group("Parentheses",
        closed("(", ")"))

    val exponent = group("Exponent",
        infix("^"))
    val multiplication = group("Multiplication",
        infix("*"),
        infix("/"),
        infix("mod"))
    val negation = group("Negation",
        prefix("-"))
    val addition = group("Addition",
        infix("+"),
        infix("-"))

    val comparison = group("Comparison",
        infix("<"),
        infix(">"))
    val equality = group("Equality",
        infix("="),
        infix("≠"))
    val not = group("Not",
        prefix("!"))
    val and = group("And",
        infix("&"))
    val or = group("Or",
        infix("|"))

    graph(or, and, not, equality, comparison, addition, multiplication, exponent, negation, parentheses, Tightest)(
      or -> List(and, not, equality, comparison, parentheses),
      and -> List(not, equality, comparison, parentheses),
      not -> List(equality, comparison, parentheses),

      equality -> List(comparison, addition, multiplication, exponent, negation, parentheses),
      comparison -> List(addition, multiplication, exponent, negation, parentheses),

      addition -> List(multiplication, exponent, negation, parentheses),
      multiplication -> List(exponent, negation, parentheses),
      exponent -> List(negation, parentheses),
      negation -> List(parentheses)
    )
  }

  def main(args: Array[String]) {
    val input = "val x := -2 * 2 ^ -4 + 7 + 5 + 6 - 17 < 6 = - 14 < 3 + 5"
    val tokens = new lexical.Scanner(input)
    val res = phrase(expr)(tokens)
    println(res)
    res match {
      case Success(x, _) => println("ExprTree: " + x)
      case Failure(x, y) =>
    }
  }

}