package scala

class Rational(n: Int, d: Int) extends Ordered[Rational] {
    require(d != 0)
    
    private val g = gcd(n.abs, d.abs)
    private val numer = (if (d<0) -n else n) / g
    private val denom = d.abs / g
    
    private val wasSimple: Boolean = (g == 1) && (d > 0)
    
    def this(n: Int) = this(n, 1)
    
    def this(tup: (Int, Int)) = this(tup._1, tup._2)
    
    def + (that: Rational): Rational =
        new Rational(
            numer * that.denom + that.numer * denom,
            denom * that.denom
        )
    
    def - (that: Rational): Rational =
        new Rational(
            numer * that.denom - that.numer * denom,
            denom * that.denom
        )
    
    def - (i: Int): Rational =
        new Rational(numer - i * denom, denom)
    
    def * (that: Rational): Rational =
        new Rational(numer * that.numer, denom * that.denom)
    
    def * (i: Int): Rational =
        new Rational(numer * i, denom)
    
    def / (that: Rational): Rational =
        new Rational(numer * that.denom, denom * that.numer)
    
    def / (i: Int): Rational =
        new Rational(numer, denom * i)
    
    def mod1: Rational =
        new Rational(((numer % denom) + denom) % denom, denom)
    
    private def gcd(a: Int, b: Int): Int =
        if (b == 0) a else gcd(b, a % b)
    
    override def equals(other: Any): Boolean =
        other match {
  
            case that: Rational =>
                (that canEqual this) &&
                numer == that.numer &&
                denom == that.denom
  
            case _ => false
        }
  
    def canEqual(other: Any): Boolean =
        other.isInstanceOf[Rational]
  
    override def hashCode: Int =
        41 * (
            41 + numer
        ) + denom
    
    def compare(that: Rational) = this.numer * that.denom - that.numer * this.denom
  
    override def toString =
        if (denom == 1) numer.toString else numer.toString +"/"+ denom.toString
}

object Rational {
    def iterator = Iterator(new Rational(0)) ++
        InfiniteIterator.cross(InfiniteIterator(Iterator.from(1), Iterator.from(-1,-1)), Iterator.from(1)).map(new Rational(_)).filter(_.wasSimple)
    
    def intervalOneZeroIterator = Iterator(new Rational(0)) ++
        Iterator.from(2).flatten((n:Int) => (1 until n).map((m: Int) => (m,n))).map(new Rational(_)).filter(_.wasSimple)
}