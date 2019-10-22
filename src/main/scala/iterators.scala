package scala

trait InfiniteIterator[+A] extends Iterator[A] {
    def hasNext = true
    override def map[B] (f: A => B) = {
        val parent = this
        new InfiniteIterator[B] {
            def next() = f(parent.next())
        }
    }
}

object InfiniteIterator {
    // beware of implicits. Should be safe here, since scope is limited to the InfiniteIterator object
    import scala.language.implicitConversions
    implicit def makeiteratorinfinite[A] (it: Iterator[A]) = new InfiniteIterator[A] { def next() = it.next() }
    
    def single[A] (x: A) = new InfiniteIterator[A] {
        val next = x
    }
    
    def apply[A] (xs: InfiniteIterator[A]*) = new InfiniteIterator[A] {
        var i = -1
        def next() = {
            i = (i+1) % xs.size
            xs(i).next()
        }
    }
    
    def concat[A] (xss: InfiniteIterator[InfiniteIterator[A]]) = new InfiniteIterator[A] {
        var i, j = -1
        val arr = new scala.collection.mutable.ArrayBuffer[InfiniteIterator[A]]
        def next() = {
            i += 1
            if (i > j) {
                i = 0
                j += 1
                arr += xss.next()
            }
            arr(i).next()
        }
    }
    
    def cross[A, B] (xs: InfiniteIterator[A], val_ys: InfiniteIterator[B]): InfiniteIterator[(A, B)] = {
        var ys = val_ys
        def augment(x: A) = {
            val (ys1, ys2) = ys.duplicate
            ys = ys1
            ys2.map((y: B) => (x, y))
        }
        concat(xs.map(augment))
    }
    
    // def naturals  = apply(Iterator.from(0))
    // def naturals_ = naturals.drop(1)
    // def integers  = apply(Iterator.from(0), Iterator.from(-1,-1))
    // def integers_ = integers.drop(1)
    // def rationals = Iterator(new Rational(0)) ++ cross(integers_, naturals_).map(new Rational(_)).filter(_.wasSimple)
}

// Old iterators - ideally get rid. Advantage is that they work interoperably with finite iterators

/*def iteratorsMix[A] (x: Iterator[A], y: Iterator[A]): Iterator[A] = new Iterator[A] {
    var tup = (y, x)
    def hasNext = tup._2.hasNext || tup._1.hasNext
    def next(): A = {tup = tup.swap; if (tup._1.hasNext) tup._1.next else tup._2.next}
}*/

/*def iteratorsInterweave[A] (xs: Iterator[Iterator[A]]): Iterator[A] = {
    if (!xs.hasNext) Iterator.empty else new Iterator[A] {
        val x = xs.next
        var switch = false
        lazy val rest = iteratorsInterweave(xs)
        def hasNext = x.hasNext || rest.hasNext
        def next() = {
            switch = !switch
            if (switch) {if (x.hasNext) x.next else rest.next}
            else {if (rest.hasNext) rest.next else x.next}
        }
    }
}*/