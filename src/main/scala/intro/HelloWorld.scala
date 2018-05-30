package intro

import scala.io.StdIn._

object HelloWorld {
  val message = "Hello world!"

  val promptUserString: String => Unit = prompt => print(prompt)

  val getName: Unit => String = _ => readLine

  val makeGreeting: String => String = name => s"Hello $name"

  val printGreeting: String => Unit = greeting => println(greeting)

  val promptForNameAndPrintGreeting: String => Unit = promptUserString andThen getName andThen makeGreeting andThen printGreeting

  def main(args: Array[String]): Unit = {
    promptForNameAndPrintGreeting("Please enter your name")
  }
}
