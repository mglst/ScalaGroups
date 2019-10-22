package scala.group

import scala.collection.immutable


trait FiniteGroup[A] extends Group[A] { supergroup =>
    final val finite = true
    override lazy val size = iterator.size
    
    def contains(x: A): Boolean = exists(_==x) // Linear. Override if more efficient implementaion exists.
    
    def printOrders: Unit = {
        val map = groupBy(order)
        val keys = map.keys.toArray.sorted
        for (k <- keys) {
            print(k.toString+" : ")
            print(map(k).mkString(", "))
            println()
        }
    }
    
    /** Prints the group's Cayley Table to the console. */
    def printCayleyTable: Unit = {
        val padLen = iterator.map(_.toString.length).max
        def pad(x : A): String = x.toString.reverse.padTo(padLen,' ').reverse
        print(" "*padLen+" | ")
        println(iterator.map(pad(_)).mkString(" "))
        println("-"*(padLen+1)+"+"+"-"*((padLen+1)*iterator.length))
        iterator.foreach((x:A) => {
            print(pad(x)+" | ")
            println(iterator.map(op(x,_)).map(pad(_)).mkString(" "))
        })
    }
    
    def printSubgroupTable: Unit = {
        val pads = iterator.map(_.toString.size).toArray
        println(iterator.mkString(" ", " ", " | " + toString))
        println("-" * (pads.sum + size) + "-+")
        for (subgroup <- subgroups) {
            val it = iterator
            val pads_it = pads.iterator
            while (it.hasNext) {
                print(" " * pads_it.next())
                print(if (subgroup.contains(it.next())) "X" else " ")
            }
            println(" | "+subgroup.toString)
        }
    }
    
    lazy val subgroups: Seq[Group[A]] = {
        val memed = new MemoizedGroup(this)
        val bij = memed.bijection
        def comebaaack(sg: memed.Subgroup): Subgroup = new Subgroup {
            override def contains(x: A) = sg.contains(bij.toB(x))
            def iterator = sg.iterator.map(bij.toA)
            val generatingSets = Set.empty[Set[A]] ++ sg.generatingSets.map(_.map(bij.toA))
            def incl(x: A) = comebaaack(sg.incl(bij.toB(x)))
        }
        memed.subgroupss.map(comebaaack).toSeq.sortBy(_.size)
    }
    
    trait Subgroup extends super.Subgroup with FiniteGroup[A]
}

class TrivialGroup[A] (val id: A) extends FiniteGroup[A] {
    def inv(x: A) = id
    def op(x: A, y: A) = id
    def iterator = Iterator(id)
    override def toString = "Trivial group <" + id + ">"
}