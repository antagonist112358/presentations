/*****************************************/
/*** Higher order functions / currying ***/
/*****************************************/
type MsgTransform = String => String

def extractTokensThen(str: String, splitChar: Char = ' ') : (String => String) => Array[String]

def joinTokens(tokens: Array[String], joinStr: String = " ") : String = tokens.mkString(joinStr)

def transformTokens(str: String, f: String => String) : String = joinTokens(
  extractTokensThen(str)(f)
    .filterNot(_.isEmpty)
)

def debugPrinter(str: String) : String = {
  println(s"Debug: $str")
  return str
}

val debugPrint: MsgTransform = debugPrinter
val capitalize : MsgTransform = transformTokens(_, s => s.toLowerCase.capitalize)
val removeWords: MsgTransform = transformTokens(_, { s =>
  val pronouns = List("the", "we", "us", "me", "you", "he", "she")
  if (pronouns.contains(s.toLowerCase)) "" else s
})

val pipeline = List(
  removeWords,
  capitalize,
  debugPrint
)

def processInputText(text: String, operations: Seq[MsgTransform]) : String = operations.foldLeft(text) { (txt, op) => op(txt) }
val processor: String => String = processInputText(_, pipeline).trim()

val processInput = "She said that he saw The QUICK brown fOx jump over the LAzY dogs Back"

println(s"Output: ${processor(processInput)}")



/************************************/
/*** Lazy evaluation and closures ***/
/************************************/
var loggingEnabled = false
def log(msg: => String) : Unit = if (loggingEnabled) println(msg)

log("Some really time consuming debug")

loggingEnabled = true
log("Some really important debug statement")

lazy val theMeaningOfLife = {
  Thread.sleep(500 * 1000) // Five hundred seconds
  42 // Wait ...it only takes 500 seconds to compute the meaning of life??!
}


/************************/
/*** Pattern Matching ***/
/************************/
def capitalize(str: String) : String = {
  if (str == null || str.isEmpty) return ""
  else if (str(0).isUpper) return str
  else {
    val firstChar = str(0)
    val rest = str.substring(1)

    return firstChar.toUpper + rest
  }

}

def capitalizePat(text: String) : String = text match {
  case "" => ""
  case str if str(0).isUpper => str
  case str => str(0).toUpper + str.substring(1)
}

import scala.util.{Try, Success, Failure}

val nonErrorOp = Try { 6 / 2 }
val errorOp = Try { 15 / 0 }

val reportResult: Try[Int] => Unit = {
  case Success(value) => println(s"Result: $value")
  case Failure(err) => println(s"Error: $err")
}

reportResult(nonErrorOp)
reportResult(errorOp)