object Nat {

  sealed abstract class Nat

  case object Zero extends Nat

  case class Succ(n: Nat) extends Nat

  def main(args: Array[String]): Unit = {

    val nat0 = Zero
    val nat1 = Succ(Zero)
    val nat2 = Succ(Succ(Zero))
    val nat3 = Succ(Succ(Succ(Zero)))
    val nat4 = Succ(Succ(Succ(Succ(Zero))))
    val nat5 = Succ(Succ(Succ(Succ(Succ(Zero)))))

    println(s"Decode: ${nat3} -> ${decode(nat3)}")

    println(s"Encode: ${0} -> ${encode(0)}")
    println(s"Encode: ${1} -> ${encode(1)}")
    println(s"Encode: ${4} -> ${encode(4)}")
    //println(s"Encode: ${-1} -> ${encode(-1)}")

    println(s"Add: ${nat2}, ${nat5} -> ${add(nat2, nat5)}")
    println(s"Add: ${nat3}, ${nat1} -> ${add(nat3, nat1)}")
    println(s"Add: ${nat0}, ${nat2} -> ${add(nat0, nat2)}")
    println(s"Add: ${nat2}, ${nat0} -> ${add(nat2, nat0)}")

    println(s"Mult: ${nat2}, ${nat5} -> ${mult(nat2, nat5)}")
    println(s"Mult: ${nat3}, ${nat1} -> ${mult(nat3, nat1)}")
    println(s"Mult: ${nat0}, ${nat2} -> ${mult(nat0, nat2)}")
    println(s"Mult: ${nat2}, ${nat0} -> ${mult(nat2, nat0)}")

    println(s"Pow: ${nat2}, ${nat5} -> ${pow(nat2, nat5)}")
    println(s"Pow: ${nat3}, ${nat1} -> ${pow(nat3, nat1)}")
    println(s"Pow: ${nat0}, ${nat2} -> ${pow(nat0, nat2)}")
    println(s"Pow: ${nat2}, ${nat0} -> ${pow(nat2, nat0)}")

    println(s"Decrement: ${nat0} -> ${decrement(nat0)}")
    println(s"Decrement: ${nat1} -> ${decrement(nat1)}")
    println(s"Decrement: ${nat4} -> ${decrement(nat4)}")

  }

  def decode(n: Nat): Int = n match
    case Zero => 0
    case Succ(n) => 1 + decode(n)

  def encode(i: Int): Nat = {
    if i < 0 then throw RuntimeException(s"i must not be negative, ${i}")
    if i == 0 then Zero
    else Succ(encode(i-1))
  }

  def add(a: Nat, b: Nat): Nat = a match
    case Zero => b
    case Succ(n) => Succ(add(n, b))

  def mult(a: Nat, b: Nat): Nat = a match
    case Zero => Zero
    case Succ(n) => add(b, mult(n, b))

  def pow(a: Nat, b: Nat): Nat = b match
    case Zero => Succ(Zero)
    case Succ(n) => mult(a, pow(a, n))

  def decrement(n: Nat): Nat = n match
    case Zero => Zero
    case Succ(n) => n

}
