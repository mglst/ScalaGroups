package scala.group.instance

import scala.group._
import scala.collection.{mutable, immutable}

final class Permutation private (val arr: Array[Int]) extends immutable.Map[Int, Int] { // make arr private!
    val n = arr.length
    
    private lazy val decomp = Permutation.decompose(this)

    override val toString = {
        if (decomp.size == 0) "e"
        else decomp.iterator.map(_.map(_+1).mkString("(","",")")).mkString("")
    }
    
    def iterator = (0 until n).map((x: Int) => (x,arr(x))).iterator
    
    def toMap = immutable.Map.from(iterator)
    
    def removed(key: Int) = toMap.removed(key)
    
    def updated[V >: Int](key: Int, value: V) = toMap.updated(key, value)
    
    def get(key: Int): Option[Int] = {
        if (key < 0 || key >= n) None
        else Some(arr(key))
    }
    
    override def equals(other: Any): Boolean = other match {
        case that: Permutation => {
            if (this.n == that.n) {
                var i = 0
                while (i < n && this.arr(i) == that.arr(i)) i += 1
                i == n
            } else false
        }
        case _ => ???
        // case that: immutable.Map[Int, Int] => ??? // Read up chapter 28, figure out how this should be done properly
    }
    
    override val hashCode: Int = arr.toSeq.hashCode
    
    lazy val inverse = {
        val narr = new Array[Int](n)
        for (i <- 0 until n) narr(arr(i)) = i
        new Permutation(narr)
    }
    
    def compose(that: Permutation): Permutation = {
        val nn = scala.math.max(this.n, that.n)
        val narr = new Array[Int](nn)
        if (this.n < that.n) {
            for (i <- 0 until this.n) narr(i) = that.arr(this.arr(i))
            for (i <- this.n until nn) narr(i) = that.arr(i)
        } else {
            for (i <- 0 until nn) {
                val t = this.arr(i)
                narr(i) = if (t >= that.n) t else that.arr(t)
            }
        }
        Permutation.shorten(narr, nn)
    }
}

object Permutation {
    private def decompose(perm: Permutation): immutable.Queue[immutable.Queue[Int]] = {
        var decomp = immutable.Queue.empty[immutable.Queue[Int]]
        val used = new Array[Boolean](perm.n)
        var start = 0
        while (start < perm.n){
            var cycle = immutable.Queue.empty[Int]
            var x = start
            while (!used(x)){
                used(x) = true
                cycle = cycle appended x
                x = perm.arr(x)
            }
            assert(x == start)
            if (cycle.size > 1) decomp = decomp appended cycle
            while (start < perm.n && used(start)) start += 1
        }
        decomp
    }
    
    val empty: Permutation = new Permutation(Array.empty[Int])
    
    /** Creates a [[https://en.wikipedia.org/wiki/Cyclic_permutation simple cycle]] permutation.
      *
      * `cycle(7,2,3)` creates a permutation which maps `7` to `2`, `2` to `3`, and `3` to `7`.
      *
      * @throws IllegalArgumentException if a number is non-positive or apears more than once
      * */
    def cycle(xs: Int*): Permutation = {
        val n = xs.max
        val arr = Array.range(0, n)
        val used = new Array[Boolean](n)
        require(xs.forall(0 < _))
        for (i <- 0 until (xs.length - 1)) {
            require(!used(xs(i) - 1))
            used(xs(i) - 1) = true
            arr(xs(i) - 1) = xs(i + 1) - 1
        }
        require(!used(xs(xs.length - 1) - 1))
        used(xs(xs.length - 1) - 1) = true
        arr(xs(xs.length - 1) - 1) = xs(0) - 1
        new Permutation(arr)
    }
    
    /** Creates a [[https://en.wikipedia.org/wiki/Permutation transposition]] permutation. */
    def transposition(a: Int, b: Int): Permutation = {
        require(a > 0 && b > 0)
        val n = scala.math.max(a, b)
        val arr = Array.range(0, n)
        arr(a-1) = b-1
        arr(b-1) = a-1
        new Permutation(arr)
    }
    
    def permutations(n: Int): Iterator[Permutation] = {
        Array.range(0, n).permutations.map(shorten(_:Array[Int], n))
    }
    
    private def shorten(arr: Array[Int], n: Int): Permutation = {
        var m = n-1
        while (m >= 0 && arr(m) == m) m -= 1
        new Permutation(arr.take(m + 1))
    }
}

object Sn extends (Int => FiniteGroup[Permutation]) {
    val map = collection.mutable.Map[Int, GSymmetry]()
    def apply(n: Int) = map.getOrElseUpdate(n, new GSymmetry(n))
    
    private[Sn] class GSymmetry (val n: Int) extends FiniteGroup[Permutation] {
        // val map = collection.mutable.Map[Int, Permutation]()
        // def elem(arr: Array[Int]) = map.getOrElseUpdate(hash(arr), new Permutation(n, arr))
        // def hash(arr: Array[Int]) = arr.fold(0)(10*_+_)
        
        val id = Permutation.empty //elem(Array.range(0, n))
        def inv(x: Permutation) = x.inverse
        def op(x: Permutation, y:Permutation) = x.compose(y)
    
        override def contains(x: Permutation) = x.n <= n
        def iterator = Permutation.permutations(n)
        
        override def toString = "S"+n.toString
    }
}