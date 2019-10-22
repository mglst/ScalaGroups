package scala.group

import scala.language.implicitConversions

/*
Groups should be:
- Variance?
- Have a nice naming scheme! Make toString be consistent / give other string things
*/

/** A generic trait for immutable groups.
  *
  * A group is a set equipped with a binary operation that combines any two elements to form a third element in such a way that four conditions called group axioms are satisfied, namely closure, associativity, identity and invertibility.
  * Examples of groups can be found in package [[scala.group.instance]] */
trait Group[A] extends Iterable[A] { supergroup =>
    /** The identity element of the group */
    val id: A
    /** */
    def inv(x: A): A
    def op(x: A, y: A): A
    val finite: Boolean // Switch to FINITE | COUNTABLE | UNCOUNTABLE ?
    def iterator: Iterator[A]
    def contains(x: A): Boolean
    
    // Pre: The order of x is finite
    def order(x: A): Int = 1 + Iterator.iterate(x)(op(x,_)).takeWhile(_!=id).length
    
    // Pre: forall x: A such that parent.contains(x) holds, then bi.AIsDefined(x) also holds
    def biject[B] (bi: Bijection[A, B]): Group[B] = {
        val parent = this
        new Group[B] {
            val id: B = bi.toB(parent.id)
            def inv(x: B): B = bi.toB(parent.inv(bi.toA(x)))
            def op(x: B, y: B): B = bi.toB(parent.op(bi.toA(x), bi.toA(y)))
            val finite: Boolean = parent.finite
            
            def iterator: Iterator[B] = parent.iterator.map(bi.toB)
            override def contains(x: B) = bi.BIsDefined(x) && parent.contains(bi.toA(x))
            
            override def toString = "this group was initialized using the bijection "+bi.toString+" on the group "+supergroup.toString
        }
    }
    
    // NOW AN INNER CLASS
    trait Subgroup extends Group[A] with Set[A] {
        // Implementations of (some) members of Group[A]. Missing are 'iterator', 'contains',  and 'finite'
        val id = supergroup.id
        def inv(x: A) = supergroup.inv(x)
        def op(x: A, y: A) = supergroup.op(x, y)
        
        // New members, specific to Subgroup[A]
        def excl(elem: A): Set[A] = toSet.excl(elem)
        
        // A subset of all generating sets for the subgroup
        val generatingSets: Set[Set[A]]
        
        override lazy val toString = { // maybe better to have as def, or computed alongside generatingSets...
            def op(pair: ((Int, Set[A]), Set[A])) = {
                val size = pair._2.size
                if (size < pair._1._1) (size, pair._2)
                else pair._1
            }
            var gset = generatingSets.foldLeft((Int.MaxValue, Set.empty[A]))(op(_, _))._2
            if (gset.isEmpty) gset = Set(id)
            gset.mkString("<", ", ", ">")
        }
    }
}