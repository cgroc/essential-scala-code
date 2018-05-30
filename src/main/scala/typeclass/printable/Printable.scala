package typeclass.printable

//Printable type class library

// Part 1: The type class
trait Printable[A] {

  def format(value: A): String
}

// Part 2: the type class instances
object PrintableInstances {

  implicit val stringPrintable: Printable[String] = new Printable[String] {

    def format(value: String): String = value
  }

  implicit val intPrintable: Printable[Int] = new Printable[Int] {

    def format(value: Int): String = value.toString
  }

  implicit def listPrintable[A](implicit printer: Printable[A]): Printable[List[A]] = new Printable[List[A]] {
    def format(list: List[A]): String = {
      def loop(l: List[A], current: String): String =
        l match {
          case Nil => current
          case head :: Nil => loop(Nil, current + printer.format(head))
          case head :: tail => loop(tail, current + printer.format(head) + ", ")
        }

      "[ " + loop(list, "") + " ]"
    }
  }

  implicit def tuplePrintable[A, B](implicit aPrinter: Printable[A], bPrinter: Printable[B]): Printable[(A, B)] = new Printable[(A, B)] {
    def format(tuple: (A, B)): String = "( " + aPrinter.format(tuple._1) + ", " + bPrinter.format(tuple._2) + " )"
  }

  implicit def mapPrintable[A, B](implicit tuplePrinter: Printable[(A, B)], listPrinter: Printable[List[(A, B)]]): Printable[Map[A, B]] = new Printable[Map[A, B]] {
    def format(map: Map[A, B]): String = listPrinter.format(map.toList)
  }
}

// Part 3: the interfaces
object Printable {

  def format[A](value: A)(implicit printable: Printable[A]): String = printable.format(value)

  def print[A](value: A)(implicit printable: Printable[A]): Unit = println(format(value))

//  def formatList[A](list: List[A])(implicit printable: Printable[A]): String = {
//    def loop(l: List[A], current: String): String =
//      l match {
//        case Nil => current
//        case head :: Nil => loop(Nil, current + printable.format(head))
//        case head :: tail => loop(tail, current + printable.format(head) + ", ")
//      }
//
//    "[ " + loop(list, "") + " ]"
//  }
//
//
//  def printList[A](list: List[A])(implicit printable: Printable[A]): Unit =
//    println(formatList(list))

}

// Domain types

final case class Cat(name: String, age: Int, colour: String)

object Cat {

  implicit val catPrintable: Printable[Cat] = new Printable[Cat] {
    override def format(cat: Cat): String = s"${cat.name} is a ${cat.age} year old ${cat.colour} cat."
  }
}

object Main extends App {

  import PrintableInstances._

  Printable.print("Well hello!")
  Printable.print(7)

  val milly = Cat("Mildred", 7, "black")
  val edna = Cat("Edna", 6, "tabby")

  Printable.print(milly)
  Printable.print(edna)

//  Printable.printList(List(milly, edna)) // okay, kind of lame to have another interface just for lists...

  Printable.print(List(milly, edna)) // ah, much better

  Printable.print((milly, "A cat!"))

  Printable.print(Map(
    1 -> milly,
    2 -> edna
  ))
}