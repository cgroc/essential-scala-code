package fn.firstorder

import scala.annotation.tailrec

object FirstOrder {
  val sumSquares: (Int, Int) => Int = (a, b) => (a * a) + (b * b)

  val anotherSumSquares: (Int, Int) => Int = (a, b )=> {
    def sq(i: Int): Int = i * i
    sq(a) + sq(b)
  } // nope

  val yetAnotherSumSquares: (Int, Int) => Int = (a, b) => Math.pow(a + b, 2).toInt - (2 * a * b)


  val longer: (String, String) => String = (s1, s2) => if(s1.length > s2.length) s1 else s2

//  val factorial: Int => Int = int => {
//    @tailrec
//    def loop(i: Int, acc: Int): Int =
//      if(i < 1) acc else loop(i - 1, acc * i)
//
//    loop(int, 1)
//  }

  val factorial: Int => Int = int => {
    if(int <= 1) 1
    else int * factorial(int - 1)
  }

}

object Main extends App {
  import FirstOrder._

   println("""sumSquares(3, 4) == """ + sumSquares(3, 4))
   println("""anotherSumSquares(3, 4) == """ + anotherSumSquares(3, 4))
   println("""yetAnotherSumSquares(3, 4) == """ + yetAnotherSumSquares(3, 4))


   println("""longer("fooo", "bar") == """ + longer("fooo", "bar"))
   println("""factorial(5) == """ + factorial(5))
}
