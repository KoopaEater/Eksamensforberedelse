import scala.annotation.tailrec

object IntList {
  sealed abstract class IntList

  case object Nil extends IntList

  case class Cons(x: Int, xs: IntList) extends IntList


  def main(args: Array[String]): Unit = {
    val il1 = Cons(2, Cons(5, Cons(3, Nil)))
    println(s"Square: ${il1} -> ${square(il1)}" )

    val il2 = Cons(2, Cons(5, Cons(3, Nil)))
    val il3 = Cons(2, Cons(3, Cons(5, Nil)))
    val il4 = Cons(2, Cons(3, Cons(5, Cons(9, Cons(10, Cons(1, Nil))))))
    val il5 = Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Cons(6, Nil))))))
    println(s"Ordered: ${il2} -> ${ordered(il2)}")
    println(s"Ordered: ${il3} -> ${ordered(il3)}")
    println(s"Ordered: ${il4} -> ${ordered(il4)}")
    println(s"Ordered: ${il5} -> ${ordered(il5)}")

    println(s"Odd: ${il2} -> ${odd(il2)}")
    println(s"Odd: ${il3} -> ${odd(il3)}")
    println(s"Odd: ${il4} -> ${odd(il4)}")
    println(s"Odd: ${il5} -> ${odd(il5)}")

    println(s"Concat: ${il2} ${il4} -> ${concat(il2, il4)}")


  }

  def square(il: IntList): IntList = il match
    case Nil => Nil
    case Cons(x, xs) => Cons(x*x, square(xs))

  @tailrec
  def ordered(il: IntList): Boolean = il match
    case Nil => true
    case Cons(x, Nil) => true
    case Cons(x, xs@Cons(y, ys)) => x <= y && ordered(xs)

  def odd(il: IntList): IntList = il match
    case Nil => Nil
    case xs@Cons(x, Nil) => xs
    case Cons(x, Cons(y, ys)) => Cons(x, odd(ys))

  def append(il: IntList, x: Int): IntList = il match
    case Nil => Cons(x, Nil)
    case Cons(y, ys) => Cons(y, append(ys, x))

  def concat(a: IntList, b: IntList): IntList = a match
    case Nil => b
    case Cons(x, xs) => Cons(x, concat(xs, b))


}
