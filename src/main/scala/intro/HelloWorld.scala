package intro

import scala.io.StdIn._

object HelloWorld {

  final case class UserPrompt(message: String)

  final case class UserName(name: String)

  final case class UserGreeting(message: String)

  val message = "Hello world!"

  val promptUserString: UserPrompt => Unit = prompt => print(prompt.message)

  val getName: Unit => UserName = _ => UserName(readLine)

  val makeGreeting: UserName => UserGreeting = userName => UserGreeting(s"Hello ${userName.name}")

  val printGreeting: UserGreeting => Unit = greeting => println(greeting.message)

  val promptForNameAndPrintGreeting: UserPrompt => Unit = promptUserString andThen getName andThen makeGreeting andThen printGreeting

  def main(args: Array[String]): Unit = {
    promptForNameAndPrintGreeting(UserPrompt("Please enter your name: "))
  }
}
