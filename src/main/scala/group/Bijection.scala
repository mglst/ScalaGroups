package scala.group

import scala.collection.immutable
import scala.collection.mutable


trait Bijection[A, B] { bij =>
    // Specifies an invertible function f from X -> Y where X, Y are subsets of A, B respectively
    /* Post: return x € X / y € Y */
    def AIsDefined(x: A): Boolean
    def BIsDefined(y: B): Boolean
    /*Pre: x € X / y € Y
     *Post: returns f(x) / f'(y) */
    def toB(x: A): B
    def toA(y: B): A
    
    lazy val inverse: Bijection[B, A] = new Bijection[B, A] {
        def BIsDefined(x: A) = bij.AIsDefined(x)
        def AIsDefined(y: B) = bij.BIsDefined(y)
        def toB(y: B) = bij.toA(y)
        def toA(x: A) = bij.toB(x)
        override lazy val inverse = bij
    }
}