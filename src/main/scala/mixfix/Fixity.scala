package mixfix

/**
 * Operator fixity: Prefix | Infix(Associativity) | Postfix | Closed
 */
sealed trait Fixity

/**
 * Prefix operator
 * 
 * not a
 * if a then b
 */
case object Prefix extends Fixity

/**
 * Infix operator
 * 
 * a and b
 * a if b else c
 */
case class Infix(associativity: Associativity) extends Fixity

/**
 * Postfix operator
 * 
 * a !
 */
case object Postfix extends Fixity

/**
 * Closed operator
 * 
 * (a)
 * |a|
 * [a]
 */
case object Closed extends Fixity
