package scala.group.instance

import scala.group._

object Q extends InfiniteGroup[Rational] {
    val id = new Rational(0)
    def inv(x: Rational) = id - x
    def op(x: Rational, y: Rational) = x + y
    def iterator = Rational.iterator
    def contains(x: Rational) = true
}

object Q$N extends InfiniteGroup[Rational] {
    val id = new Rational(0)
    def inv(x: Rational) = (new Rational(1) - x).mod1
    def op(x: Rational, y: Rational) = (x + y).mod1
    def iterator = Rational.intervalOneZeroIterator
    def contains(x: Rational) = id <= x && x < new Rational(1)
}