import IntList._

object IntTree {

  sealed abstract class IntTree

  case object Leaf extends IntTree

  case class Branch(left: IntTree, x: Int, right: IntTree) extends IntTree

  def main(args: Array[String]): Unit = {

    val tree1 = insert(insert(insert(insert(Leaf, 1), 0), 3), 2)

    println(s"Size: ${tree1} -> ${size(tree1)}")
    println(s"Flatten: ${tree1} -> ${flatten(tree1)}")
    println(s"Contains: ${tree1} ${0} -> ${contains(tree1, 0)}")
    println(s"Contains: ${tree1} ${1} -> ${contains(tree1, 1)}")
    println(s"Contains: ${tree1} ${2} -> ${contains(tree1, 2)}")
    println(s"Contains: ${tree1} ${3} -> ${contains(tree1, 3)}")
    println(s"Contains: ${tree1} ${4} -> ${contains(tree1, 4)}")
  }



  def size(t: IntTree): Int = t match
    case Leaf => 0
    case Branch(left, _, right) => size(left) + 1 + size(right)

  def height(t: IntTree): Int = t match {
    case Leaf => 0
    case Branch(left, _, right) => (height(left) max height(right)) + 1
  }

  def flatten(t: IntTree): IntList = t match
    case Leaf => Nil
    case Branch(left, x, right) => concat(append(flatten(left), x), flatten(right))

  def ordered(t: IntTree): Boolean = {
    def ord(t: IntTree, minval: Int, maxval: Int): Boolean = t match {
      case Branch(left, x, right) => ord(left, minval, x) & ord(right, x, maxval)
      case Leaf => minval <= maxval
    }

    ord(t, Int.MinValue, Int.MaxValue)
  }

  def insert(t: IntTree, x: Int): IntTree = t match {
    case Leaf => Branch(Leaf, x, Leaf)
    case Branch(left, y, right) =>
      if (x <= y) Branch(insert(left, x), y, right)
      else Branch(left, y, insert(right, x))
  }

  def contains(t: IntTree, x: Int): Boolean = t match
    case Leaf => false
    case Branch(left, y, right) => y == x || (if (x < y) contains(left, x) else contains(right, x))

}
