package counter

// TODO: Implement a Counter class with:
//  - a construct that accepts an Int value
//  - an "inc" method that increments the value
//  - a "get" method that returns the value
//  - a "toString" method that returns "Counter(x)" where "x" is the value

class Counter(value: Int) {

  def inc: Counter = new Counter(value + 1)

  def get: Int = value

  override def toString: String = s"Counter($value)"
}

object Counter {

  val empty = new Counter(0)

  def create(value: Int) = new Counter(value)

}


class Coordinate(val x: Int, val y: Int) {

  def add(other: Coordinate): Coordinate = Coordinate.add(this, other)

  def +(other: Coordinate): Coordinate = this add other

  def multiplyByScalar(scalar: Int): Coordinate = Coordinate.multiplyByScalar(this, scalar)

  def *(scalar: Int): Coordinate = this multiplyByScalar scalar

  def dotProduct(other: Coordinate): Int = Coordinate.dotProduct(this, other)

  def `.`(other: Coordinate): Int = this dotProduct other

  override def toString: String = s"Coordinate($x, $y)"

  // oops, needs to be orthogonal, this isn't going to happen as things stand
  //  def vectorProduct(other: Coordinate): Coordinate = ???
}

object Coordinate {

  def add(first: Coordinate, second: Coordinate): Coordinate = new Coordinate(first.x + second.x, first.y + second.y)

  def multiplyByScalar(c: Coordinate, scalar: Int): Coordinate = new Coordinate(c.x * scalar, c.y * scalar)

  def dotProduct(c1: Coordinate, c2: Coordinate): Int = (c1.x * c2.x) + (c1.y * c2.y)

}

object Main extends App {
  val counter1 = new Counter(41)
  val counter2 = counter1.inc
  val theAnswer = counter2.get
  println(counter1)
  println(counter2)
  println(s"The answer is $theAnswer")

  println(Counter.empty)
  println(Counter.create(7))

  val coordinate1: Coordinate = new Coordinate(1, 2)
  val coordinate2: Coordinate = new Coordinate(2, 1)
  println(coordinate1.add(coordinate2))
  println(coordinate1 + coordinate2 + coordinate1)
  println(coordinate1 * 3)
  println(coordinate1 dotProduct coordinate2)
  println(coordinate1 `.` coordinate2)

  println(Coordinate.dotProduct(coordinate1, coordinate2))


}
