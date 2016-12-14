package adt.calc

sealed trait Calc
final case class Num(a: Double) extends Calc
final case class Add(a: Calc, b: Calc) extends Calc
final case class Sub(a: Calc, b: Calc) extends Calc
final case class Mul(a: Calc, b: Calc) extends Calc
final case class Div(a: Calc, b: Calc) extends Calc
final case class Sqrt(a: Calc) extends Calc

object DoubleInterpreter {
  def run(calc: Calc): Double =
    calc match {
      case Num(a)    => a
      case Add(a, b) => run(a) + run(b)
      case Sub(a, b) => run(a) - run(b)
      case Mul(a, b) => run(a) * run(b)
      case Div(a, b) => run(a) / run(b)
      case Sqrt(a)   => math.sqrt(run(a))
    }
}

object IntInterpreter {
  def run(calc: Calc): Int =
    calc match {
      case Num(a)    => a.toInt
      case Add(a, b) => run(a) + run(b)
      case Sub(a, b) => run(a) - run(b)
      case Mul(a, b) => run(a) * run(b)
      case Div(a, b) => run(a) / run(b)
      case Sqrt(a)   => math.sqrt(run(a)).toInt
    }
}

sealed trait SafeResult
final case class Passed(value: Double) extends SafeResult
final case class Failed(message: String) extends SafeResult

object SafeInterpreter {
  def run(calc: Calc): SafeResult =
    calc match {
      case Num(a) => Passed(a)

      case Add(a, b) =>
        run(a) match {
          case Passed(a) =>
            run(b) match {
              case Passed(b) => Passed(a + b)
              case Failed(m) => Failed(m)
            }
          case Failed(m) => Failed(m)
        }

      case Sub(a, b) =>
        run(a) match {
          case Passed(a) =>
            run(b) match {
              case Passed(b) => Passed(a + b)
              case Failed(m) => Failed(m)
            }
          case Failed(m) => Failed(m)
        }

      case Mul(a, b) =>
        run(a) match {
          case Passed(a) =>
            run(b) match {
              case Passed(b) => Passed(a * b)
              case Failed(m) => Failed(m)
            }
          case Failed(m) => Failed(m)
        }

      case Div(a, b) =>
        run(a) match {
          case Passed(a) =>
            run(b) match {
              case Passed(b) =>
                if(b == 0) {
                  Failed("Divsion by zero")
                } else {
                  Passed(a / b)
                }
              case Failed(m) => Failed(m)
            }
          case Failed(m) => Failed(m)
        }

      case Sqrt(a) =>
        run(a) match {
          case Passed(a) =>
            if(a >= 0) {
              Passed(math.sqrt(a))
            } else {
              Failed("Square root of a negative number")
            }
          case Failed(m) => Failed(m)
        }

    }
}

object Main extends App {
  // TODO:
  // - Calculate and print: 1.1 + 2.2 * 3.3
  val calc1 = Add(Num(1.1), Mul(Num(2.2), Num(3.3)))
  println(DoubleInterpreter.run(calc1))
  println(IntInterpreter.run(calc1))

  // - Calculate and print: 1.1 * 2.2 + 3.3
  val calc2 = Add(Mul(Num(1.1), Num(2.2)), Num(3.3))
  println(DoubleInterpreter.run(calc2))
  println(IntInterpreter.run(calc2))

  // TODO:
  // - Implement squaring a number:
  def square(a: Double): Calc =
    Mul(Num(a), Num(a))

  // - Implement pythagoras' theorem using Calc
  def pythag(a: Double, b: Double): Calc =
    Sqrt(Add(square(a), square(b)))

  // - Use your pythagoras implementation to calculate
  //   the hypotenuse of a 3x4 triangle
  println(DoubleInterpreter.run(pythag(3, 4)))
  println(IntInterpreter.run(pythag(3, 4)))

  // Demonstrate run fails gracefully:
  println(SafeInterpreter.run(Sqrt(Num(-1))))
  println(SafeInterpreter.run(Div(Num(1), Num(0))))
  println(SafeInterpreter.run(pythag(-3, -4)))
}