package scala.ring

/** Generic trait for countable rings*/
trait Ring[A] extends Iterable[A] {
    def addid: A
    def mulid: A
    def add(x: A, y: A): A
    def mul(x: A, y: A): A
    def addinv(x: A): A
    def mulinv(x: A): A
    
}