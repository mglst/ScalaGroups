// - implement the semi-direct product

package scala.group
// import scala.group.Group

class Cross[A,B] (ga: Group[A], gb: Group[B]) extends Group[(A,B)] {
    val finite = ga.finite && gb.finite
    
    val id = (ga.id, gb.id)
    def inv(x: (A, B)) = (ga.inv(x._1), gb.inv(x._2))
    def op(x : (A, B), y : (A, B)) = (ga.op(x._1, y._1), gb.op(x._2, y._2))
    
    def iterator = {
        if (ga.finite) ga.iterator.flatMap((a: A) => gb.iterator.map((a, _: B)))
        else if (gb.finite) gb.iterator.flatMap((b: B) => ga.iterator.map((_: A, b)))
        else InfiniteIterator.cross(ga.iterator, gb.iterator)
    }
    override def contains(x : (A, B)) = ga.contains(x._1) && gb.contains(x._2)
    
    override def toString = ga.toString+" X "+gb.toString
}

// there should really be a factory constructor