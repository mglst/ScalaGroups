package scala.group

import scala.collection.immutable
import scala.collection.mutable


trait InfiniteGroup[A] extends Group[A] {
    final val finite = false
    
    /** Prints the group's Cayley Table to the console. */
    def printCayleyTable(n: Int): Unit = {
        val pad_len = iterator.take(n).map(_.toString.length).max
        def pad(x : A): String = x.toString.reverse.padTo(pad_len,' ').reverse
        print(" "*pad_len+" | ")
        println(iterator.take(n).map(pad(_)).mkString(" "))
        println("-"*(pad_len+1)+"+"+"-"*((pad_len+1)*n))
        iterator.take(n).foreach((x:A) => {
            print(pad(x)+" | ")
            println(iterator.take(n).map(op(x,_)).map(pad(_)).mkString(" "))
        })
    }
}