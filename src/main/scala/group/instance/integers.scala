package scala.group.instance

import scala.group._

/** The group of Integers under addition. */
object Z extends Group[Int] {
    val finite = false
    val id = 0
    def inv(x : Int) = -x
    def op(x : Int, y : Int) = x + y
    
    // override def orderIsFinite(x: Int) = if (x == id) true else false
    
    def iterator = InfiniteIterator(Iterator.from(0), Iterator.from(-1,-1))
    override def contains(x : Int) = true
    
    override def toString = "Z"
    
    /*override def generateSubgroup(xs: Int*) = {
        def gcd(a: Int, b: Int) : Int = if (b == 0) a else gcd(b, a % b)
        val n = xs.map(scala.math.abs).fold(0)(gcd)
        val parent = this
        if (n == 0) new TrivialGroup(0)
        else if (n ==  1) parent // Optional
        else {
            val b = new Bijection[Int, Int] {
                def AIsDefined(x: Int) = true
                def BIsDefined(x: Int) = (x % n) == 0
                def toA(x: Int) = x / n
                def toB(x: Int) = x * n
            }
            biject(b)
        }
    }*/
}

/** Factory for the cyclic group of a specified order, where the elements are represented as integers. */
object Zn extends (Int => FiniteGroup[Int]) {
    val hashmap = scala.collection.mutable.Map.empty[Int, ModGroup]
    def apply(n : Int) = hashmap.getOrElseUpdate(n, new ModGroup(n))
    
    private[Zn] class ModGroup (n : Int) extends FiniteGroup[Int] {
        require(n>0)
        
        val id = 0
        def inv(x : Int) = (n-x) % n
        def op(x : Int, y : Int) = (x+y) % n
        
        def iterator = (0 until n).iterator
        override def contains(x : Int) = x >= 0 && x < n
        
        override def toString = "Z" + n
    }
}