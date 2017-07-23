/*********************/
/*** Continuations ***/
/*********************/
object Calculator {
  import scala.io._

  type IntFnc = (Int, Int) => Int

  def getInput(cont: Int => Unit) : Unit = {
    print("Enter first integer: ")
    cont(StdIn.readLine().toInt)
  }

  def getOperation(firstNum: Int, cont: (Int, IntFnc) => Unit) : Unit = {
    print("Enter an operator: ")
    val opFnc: IntFnc = StdIn.readLine match {
      case "+" => (x, y) => x + y
      case "-" => (x, y) => x - y
      case "*" => (x, y) => x * y
      case "/" => (x, y) => x / y
      // Todo: Add Modulo
      case invalidOp => throw new RuntimeException(s"Unknown integer operation: $invalidOp")
    }

    cont(firstNum, opFnc)
  }

  def getSecondInput(input: (Int, IntFnc), cont: (Int, IntFnc, Int) => Unit) : Unit = input match {
    case (x, fnc) =>
      print("Enter second integer: ")
      val y = StdIn.readLine().toInt
      cont(x, fnc, y)
  }

  def computeResult[T](opAndInputs: (Int, IntFnc, Int), cont: Int => Unit) : Unit = opAndInputs match {
    case (x, fnc, y) =>
      val result = fnc(x, y)
      cont(result)
  }

  def displayResult(result: Int, cont: => Unit) : Unit = {
    println(s"Result: $result")
    cont
  }

  def shouldContinue(cont: => Unit) : Unit = {
    print("\nRun again? ")
    StdIn.readLine.toLowerCase match {
      case "" | "n" | "no" => ()
      case "y" | "yes" => cont
    }
  }

  def main(args: Array[String]) : Unit = {
    lazy val calculator : Unit =
      getInput(x =>
        getOperation(x, (x, fnc) =>
          getSecondInput((x, fnc), (x, fnc, y) =>
            computeResult((x, fnc, y), res =>
              displayResult(res,
                shouldContinue(calculator)
              )
            )
          )
        )
      )
    calculator
  }

}
