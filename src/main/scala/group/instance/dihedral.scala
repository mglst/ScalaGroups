package scala.group.instance

import scala.group._
import Dihedral.{Diso, Paramdihedral}

// Dihedral Group

// Diso needs to be reworked, similar to Rational.
// r and s should be private, there should be invert & compose methods

trait Dihedral
object Dihedral {
    class Diso (val r: Int, val s: Boolean) {
        override def equals(other: Any): Boolean = other match {
            case that: Diso => this.r == that.r && this.s == that.s
            case _ => false
        }
        override def hashCode = r + (if (s) 1997 else 0)
        
        override val toString = (r, s) match {
            case (0, false) => "e"
            case (0, true)  => "s"
            case (1, false) => "r"
            case (1, true)  => "rs"
            case (r, false) => "r"+(if (r < 0) "("+r+")" else r)
            case (r, true)  => "r"+(if (r < 0) "("+r+")" else r)+"s"
        }
    }
    class Paramdihedral (val n: Int) extends FiniteGroup[Diso] with ((Int, Boolean) => Diso) {
        require(n>0)
        
        def apply(r: Int, s: Boolean) = {
            require(0 < r && r <= n)
            new Diso(r, s)
        }
    
        val id = new Diso(0, false)
        def inv(a: Diso) = new Diso(if (a.s) a.r else (n-a.r)%n, a.s)
        def op(a: Diso, b:Diso) = new Diso(if (a.s) (n+a.r-b.r)%n else (a.r+b.r)%n, a.s^b.s)
        
        override def contains(a: Diso) = 0 <= a.r && a.r < n
        def iterator = Iterator.range(0, n).map(new Diso(_,false)) ++ Iterator.range(0, n).map(new Diso(_,true))
        
        override val toString = "D" + n.toString
    }
}

/** Factory for [Dihedral] instances. */
object Dn extends Dihedral with (Int => Paramdihedral) {
    val hashmap = scala.collection.mutable.Map.empty[Int, Paramdihedral]
    def apply(n: Int) = hashmap.getOrElseUpdate(n, new Paramdihedral(n))
}


// Infinite Dihedral Group

object Doo extends Dihedral with Group[Diso] with ((Int, Boolean) => Diso) {
    def apply(r: Int, s: Boolean) = new Diso(r, s)
    
    val finite = false
    val id = new Diso(0, false)
    def inv(a: Diso) = new Diso(if (a.s) a.r else -a.r, a.s)
    def op(a: Diso, b:Diso) = new Diso(if (a.s) a.r - b.r else a.r + b.r, a.s ^ b.s)
    
    override def contains(a: Diso) = true
    def iterator = InfiniteIterator(
        Z.iterator.map(new Diso(_,false)),
        Z.iterator.map(new Diso(_,true)))
}