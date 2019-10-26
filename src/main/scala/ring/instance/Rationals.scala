package scala.ring.instance

import scala.ring._

object Q extends Ring[Rational] {
    val addid = new Rational(0)
    val mulid = new Rational(1)
    def add(x: Rational, y: Rational) = x + y
    def mul(x: Rational, y: Rational) = x * y
    def addinv(x: Rational) = addid - x
    def mulinv(x: Rational) = mulid / x
    def iterator = Rational.iterator
    def contains(x: Rational) = true
}

/*object Q$N extends InfiniteGroup[Rational] {
    val id = new Rational(0)
    def inv(x: Rational) = (new Rational(1) - x).mod1
    def op(x: Rational, y: Rational) = (x + y).mod1
    def iterator = Rational.intervalOneZeroIterator
    def contains(x: Rational) = id <= x && x < new Rational(1)
}*/