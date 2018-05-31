package adt.intlist

sealed trait IntList {

  def mapInts(f: Int => Int): IntList =
    this match {
      case IntNil => IntNil
      case IntPair(h, t) => IntPair(f(h), t.mapInts(f))
    }

  def double: IntList = this.mapInts(_*2)

  def exists(f: Int => Boolean): Boolean =
    this match {
      case IntNil => false
      case IntPair(h, t) => f(h) || t.exists(f)
    }

  def forAll(f: Int => Boolean): Boolean =
    this match {
      case IntNil => true
      case IntPair(h, t) => f(h) && t.exists(f)
    }

  def allEven: Boolean = this.forAll(_ % 2 == 0)

  def fold[B](unit: B, f: (Int, B) => B): B =
    this match {
      case IntNil => unit
      case IntPair(h, t) => f(h, t.fold(unit,f))
    }

  def total: Int = this.fold[Int](0, _ + _)

  def product: Int = this.fold[Int](1, _ * _)

  def forAllFold(pred: Int => Boolean): Boolean = this.fold[Boolean](true, (i, b) => pred(i) && b)

  def allEvenFold: Boolean = this.forAllFold(_ % 2 == 0)

  def existsFold(pred: Int => Boolean): Boolean = this.fold[Boolean](false, (i, b) => pred(i) || b)

  def contains(targetInt: Int): Boolean = this.existsFold(_ == targetInt)

}

final case class IntPair(head: Int, tail: IntList) extends IntList

case object IntNil extends IntList


object Main extends App {
  val ints = IntPair(1, IntPair(2, IntPair(3, IntNil)))
  val evenInts = IntPair(2, IntPair(4, IntPair(6, IntNil)))

  val doubledInts = ints.double

  println(ints)
  println(ints.double)

  println("--- allEvens ---")
  println(ints + " : " + ints.allEven)
  println(evenInts + " : " + evenInts.allEven)
  println(evenInts + " : " + evenInts.allEvenFold)

  println("--- totals ---")
  println(ints + " : " + ints.total)
  println(evenInts + " : " + evenInts.total)

  println("--- contains ---")
  println(ints + " contains 7? : " + ints.contains(7))
  println(ints + " contains 2? : " + ints.contains(2))





  // println(ints + """.contains(1) == """ + ints.contains(1))
  // println(ints + """.contains(5) == """ + ints.contains(5))

  // println(ints + """.add(1) == """ + ints.add(1))
  // println(ints + """.add(5) == """ + ints.add(5))

  // println(ints + """.total == """ + ints.total)
}
