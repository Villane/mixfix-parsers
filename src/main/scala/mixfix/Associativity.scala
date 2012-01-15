package mixfix

/**
 * Operator associativity: Left | Right | Non
 */
sealed trait Associativity

/**
 * Left-associative: a + b + c = (a + b) + c
 */
case object Left extends Associativity

/**
 * Right-associative: a + b + c = a + (b + c)
 */
case object Right extends Associativity

/**
 * Non-associative: a + b + c = error
 */
case object Non extends Associativity
