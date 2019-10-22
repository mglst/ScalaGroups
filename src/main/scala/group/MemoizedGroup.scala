package scala.group

// import scala.collection.mutable
import scala.collection.immutable

// Potential improvements: use byte / short / int (lol groups that large) depending on the size of the group

/** Uses arrays to memoize a finite group. */
class MemoizedGroup[A] (parent: FiniteGroup[A]) extends FiniteGroup[Int] { supergroup =>
    override lazy val size = parent.size
    
    def parentiterator = Iterator(parent.id) ++ parent.iterator.filter(_ != parent.id)
    // to make things simple
    // really we can relax this condition to make sure that id has index <= 63, such that small groups (of which
    // there likely won't be that many anyways) won't take more than one long's worth of data in Subgroup's
    // BitSet
    
    val hashmap = scala.collection.immutable.HashMap.empty[A, Int] ++ parentiterator.zip(Iterator.from(0))
    
    val elemsarray = new Array(size).asInstanceOf[Array[A]] // *irony* Very nice. Well done, you.
    for (x <- parentiterator) elemsarray(hashmap(x)) = x
    
    val invarray = new Array[Int](size)
    val cayleymatrix = new Array[Array[Int]](size)
    for (i <- 0 until size) {
        invarray(i) = hashmap(parent.inv(elemsarray(i)))
        cayleymatrix(i) = new Array[Int](size)
        for (j <- 0 until size) cayleymatrix(i)(j) = hashmap(parent.op(elemsarray(i), elemsarray(j)))
    }
    
    val id = hashmap(parent.id)
    def inv(i: Int) = invarray(i)
    def op(i: Int, j: Int) = cayleymatrix(i)(j)
    
    def iterator = (0 until size).iterator
    override def contains(i: Int) = 0 <= i && i < size
    
    val bijection = new Bijection[A, Int] {
        def AIsDefined(x: A) = parent.contains(x)
        def BIsDefined(i: Int) = supergroup.contains(i)
        def toB(x: A) = hashmap(x)
        def toA(i: Int) = elemsarray(i)
    }
    
    lazy val subgroupss = {
        var subgs = immutable.Set(new Subgroup(immutable.BitSet(0), Set(Set.empty[Int]))) // Init with trivial subgroup
        for (i <- 1 until size) {
            var new_subgs = subgs
            for (subg <- subgs) {
                if (!subg.contains(i)) {
                    val new_subg = subg.incl(i) // because + is declared as final in immutable.Set
                    val new_subgs_it = new_subgs.iterator
                    var merged = false
                    while (new_subgs_it.hasNext && !merged) {
                        val test_g = new_subgs_it.next()
                        if (test_g.bitset == new_subg.bitset){ // use &~ maybe?
                            test_g.generatingSets ++ subg.generatingSets.map(_ + i)
                            merged = true
                        }
                    }
                    if (!merged) new_subgs += new_subg
                }
            }
            subgs = new_subgs
        }
        subgs
    }
    
    class Subgroup private[MemoizedGroup] (
        val bitset: immutable.BitSet,
        override val generatingSets: Set[Set[Int]])
            extends super.Subgroup {
        
        // maybe best to store generatingSets as a tree rather than an immutable set of sets
        override def toSet[B >: Int] = bitset.toSet[B]
        
        def iterator = bitset.iterator
        override def contains(i: Int) = bitset.contains(i)
        
        def incl(elem: Int): supergroup.Subgroup = if (contains(elem)) this else {
            var nbitset = bitset + elem
            var queue = immutable.Queue(elem)
            def add(i: Int): Unit = if (!nbitset.contains(i)) {
                nbitset += i
                queue :+= i
            }
            while (!queue.isEmpty){
                val i = queue.head
                queue = queue.tail
                for (j <- nbitset) {
                    add(op(i, j))
                    add(op(j, i))
                }
            }
            new supergroup.Subgroup(nbitset, generatingSets.map(_ + elem))
        }
    }
}