package generic.mylist

import scala.annotation.tailrec
import scala.language.higherKinds
import cats._

// TODO: Implement MyList
// filter, find, map, flatMap, foldLeft, foldRight

sealed trait MyList[A] {

  def exists(f: A => Boolean): Boolean =
    this match {
      case MyNil() => false
      case MyPair(h, t) => f(h) || t.exists(f)
    }

  def filter(f: A => Boolean): MyList[A] =
    this match {
      case MyNil() => MyNil()
      case MyPair(h, t) if f(h) => MyPair(h, t.filter(f))
      case MyPair(_, t) => t.filter(f)
    }

  def find(f: A => Boolean): Option[A] =
    this match {
      case MyNil() => None
      case MyPair(h, _) if f(h) => Some(h)
      case MyPair(_, t) => t.find(f)
    }

  def map[B](f: A => B): MyList[B] =
    this match {
      case MyNil() => MyNil()
      case MyPair(h, t) => MyPair(f(h), t.map(f))
    }

  def ++(list: MyList[A]): MyList[A] =
    this match {
      case MyNil() => list
      case MyPair(h, t) => MyPair(h, t ++ list)
    }

  def +(value: A): MyList[A] =
    this match {
      case MyNil() => MyPair(value, MyNil())
      case MyPair(h, t) => MyPair(h, t + value)
    }

  def flatMap[B](f: A => MyList[B]): MyList[B] =
    this match {
      case MyNil() => MyNil()
      case MyPair(h, t) => f(h) ++ t.flatMap(f)
    }

  @tailrec
  final def foldLeft[B](unit: B)(f: (B, A) => B): B =
    this match {
      case MyNil() => unit
      case MyPair(h, t) => t.foldLeft(f(unit, h))(f) //f(t.foldLeft(unit)(f), h)
    }

  def reverse: MyList[A] = {
    @tailrec
    def loop(l: MyList[A], acc: MyList[A]): MyList[A] =
      l match {
        case MyNil() => acc
        case MyPair(h, t) => loop(t, MyPair(h, acc))
      }
    loop(this, MyNil())
  }

  // TODO: In terms of foldLeft - this.reverse.foldLeft(unit)((a, b) => f(b, a))
  def foldRight[B](unit: B)(f: (A, B) => B): B =
    this.reverse.foldLeft(unit)((a, b) => f(b, a))
//    this match {
//      case MyNil() => unit
//      case MyPair(h, t) => f(h, t.foldRight(unit)(f))
//    }

  def combineAll(implicit monoid: Monoid[A]): A =
    this.foldLeft(monoid.empty)(monoid.combine)


}

case class MyPair[A](head: A, tail: MyList[A]) extends MyList[A]

case class MyNil[A]() extends MyList[A]

object Main extends App {
  val myList = MyPair(1, MyPair(2, MyPair(3, MyNil())))

  import cats.instances.int._
  println(myList.combineAll)

  val myOptionList = MyPair(Option(1), MyPair(Option(2), MyPair(Option(3), MyNil())))

  import cats.instances.option._
  println(myOptionList.combineAll)
}
