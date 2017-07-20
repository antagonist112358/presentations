import scala.annotation.tailrec

/* Scala-ish stuff - Can be ignored... */
type FibCalculator = Int => BigInt
type SubsetCalculator = (Seq[Int], Int) => Seq[Seq[Int]]

def printFib(number: Int, calculator: FibCalculator) : Unit = {
  def format(n: BigInt) = java.text.NumberFormat.getInstance.format(n)
  println(s"The $number fibonacci number is: ${format(calculator(number))}")
}

def printSubsetSum(source: Seq[Int], sum: Int, calculator: SubsetCalculator) : Unit = {
  import AnagramSearch._
  val subSums = timeFunction("Subset Sum", calculator(source, sum))
  if (subSums.isEmpty)
    println(s"Can not find any way to sum $sum from numbers [${source.mkString(", ")}]")
  else {
    println(s"Found ${subSums.size} ways to sum $sum from numbers [${source.mkString(", ")}]")
    subSums.foreach(seq => println(s"\t$seq - Sum: ${seq.sum}"))
  }
}
/* End Scala-ish stuff */



// +-------------+
// | MEMOIZATION |
// +-------------+
class Memo[A, B] {
  import scala.collection.mutable // Mutable!!?? -- WAT???
  private[this] val cache = mutable.Map.empty[A, B]
  def apply(x: A)(f: A => B) : B = cache getOrElseUpdate(x, f(x))
}



// +----------------------------------------+
// | Trampoline /  Lazy function evaluation |
// +----------------------------------------+
def isEven(n: Int) : Trampoline[Boolean] = {
  if (n == 0) Trampoline(true)
  else Trampoline(isOdd(n - 1))
}

def isOdd(n : Int) : Trampoline[Boolean] = {
  if (n == 0) Trampoline(false)
  else Trampoline(isEven(n - 1))
}


val testEven: Boolean = isEven(9998779).run




// +-----------+
// | FIBONACCI |
// +-----------+
def fibonacci(number: Int) : BigInt = {
  if (number == 0 || number == 1) number
  else {
    var last: BigInt = 1
    var acc: BigInt = 0
    for (_ <- 0 until number) {
      val tmp = acc
      acc = tmp + last
      last = tmp
    }
    acc
  }
}

def fibonacciRecSimple(n: Int) : BigInt = n match {
  case 0 | 1 => n
  case _ => fibonacciRecSimple(n - 1) + fibonacciRecSimple(n - 2)
}

def fibonacciTailRec(number: Int) : BigInt = {
  @tailrec def fibTail(n: Int, acc: BigInt, last: BigInt) : BigInt = n match {
    case 0 => acc
    case _ => fibTail(n - 1, last, acc + last)
  }

  fibTail(number, 0, 1)
}

def fibonacciTailRecMemo(number: Int) : BigInt = {
  val memo = new Memo[Int, BigInt]
  def fibMemo(n: Int, a: BigInt, b: BigInt) : BigInt = n match {
    case 0 => a
    case _ => memo(n - 1)(fibMemo(_, b, a + b))
  }

  fibMemo(number, 0, 1)
}

val fibCalc: FibCalculator = fibonacci

printFib(0, fibCalc)
printFib(1, fibCalc)
printFib(2, fibCalc)
printFib(3, fibCalc)
printFib(4, fibCalc)
printFib(15, fibCalc)
printFib(30, fibCalc)
printFib(45, fibCalc)
printFib(60, fibCalc)
printFib(100, fibCalc)




// +------------+
// | SUBSET-SUM |
// +------------+
def subsetSumSimple(numbers: Seq[Int], targetSum: Int) : Seq[Seq[Int]] = {
  import scala.collection.mutable
  val acc = mutable.LinkedHashSet.empty[Seq[Int]]

  def traverseUnit(seq: Seq[Int])(f: (Int, Seq[Int]) => Unit) : Unit = {
    for (i <- seq.indices) {
      f(seq(i), seq.slice(0, i) ++ seq.slice(i + 1, seq.length))
    }
  }

  def subsetRec(nums: Seq[Int], sumSeq: Seq[Int] = Seq()): Unit = {
    val subSum = sumSeq.sum

    if (subSum == targetSum) {
      val sorted = sumSeq.sorted
      if (!acc.contains(sorted)) acc += sorted
    } else if (nums.isEmpty) {
      ()
    } else traverseUnit(nums){ (elem, rem) =>
      subsetRec(rem, sumSeq :+ elem)
    }
  }

  subsetRec(numbers.sorted)
  acc.toSeq
}

def subsetSumMemo(numbers: Seq[Int], targetSum: Int) : Seq[Seq[Int]] = {
  val memo = new Memo[(Int, Seq[Int]), Set[Seq[Int]]]
  val len = numbers.length

  def loopMemo(sortedNumbers: Seq[Int], index: Int = 0, subSeq: Seq[Int] = Seq(), acc: Set[Seq[Int]] = Set()): Set[Seq[Int]] = {
    val subSeqSum = subSeq.sum
    val nextAcc = if (subSeqSum == targetSum) Set(subSeq) ++ acc else acc

    @inline
    def lookup(index: Int, seq: Seq[Int]) = memo((index, seq.sorted)) { case (idx, testSeq) => loopMemo(sortedNumbers, idx, testSeq, Set()) }

    if (index == len) nextAcc
    else {
      val currNum = sortedNumbers(index)
      // Try with the current element
      lookup(index + 1, currNum +: subSeq) ++
      // Try without current element
      lookup(index + 1, subSeq) ++
      // Include any results we already found
      nextAcc
    }
  }

  loopMemo(numbers.sorted).toSeq
}


val subsetCalc: SubsetCalculator = subsetSumMemo

printSubsetSum(List(1, 2, 3, 4, 5), 7, subsetCalc)
printSubsetSum(List(-1, 1, -2, 2, -3, 3, 4, 5, 9, 14), 8, subsetCalc)
printSubsetSum(List(4, 5, 6, 7, 8, 9, -1, -2, -3, 21, 11, 12, 13), 23, subsetCalc)
printSubsetSum(List(27, 32, 54, 11, 35, 50, 43, -3, -4, 7, 1, 25, 5, 27, 37, 9, 7, 60), 60, subsetCalc)