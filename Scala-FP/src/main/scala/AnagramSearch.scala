import java.time
import java.time.LocalDateTime

import scala.annotation.tailrec
import scala.collection.immutable.HashSet
import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.io.Source


object AnagramSearch {
  private val sourceText = "etaoinsrldcumetao rstlne"

  private val dictionary : Seq[String] /* Type should be: IO[Seq[String]] */ = {
    def makeSet(wordsIter: Iterator[String], acc: HashSet[String]) : HashSet[String] = if (wordsIter.hasNext) {
      makeSet(wordsIter, acc + wordsIter.next().toLowerCase)
    } else acc

    val filePath = """D:\Development\Workspaces\Scala-FP\src\main\resources\words.txt"""

    (for {
      stream <- IO(Source.fromFile(filePath))
      sets = makeSet(stream.getLines, HashSet()).toSeq
      _ = stream.close()
    } yield sets).runUnsafe match {
      case Right(dict) => dict
      case Left(err) => throw err
    }

    /*
    val stream = Source.fromFile(filePath)
    try {
      makeSet(stream.getLines, HashSet()).toSeq
    } finally {
      stream.close()
    }
    */
  }

  private val lookup = {
    def toCharCounts(text: String) = text.toCharArray.foldLeft(Map.empty[Char, Int]) { (map, c) =>
      if (c.isSpaceChar) map
      else {
        map.get(c) match {
          case Some(cnt) => map + (c -> (cnt + 1))
          case None => map + (c -> 1)
        }
      }
    }

    val srcCharCnts = toCharCounts(sourceText)

    @tailrec
    def checkCharCountsRec(toCheck: Seq[(Char, Int)]) : Boolean = toCheck match {
      case (k, v) +: tail => srcCharCnts get k match {
        case Some(cnt) => if (cnt < v) false else checkCharCountsRec(tail)
        case None => false
      }
      case Nil => true
    }

    word: String => checkCharCountsRec(toCharCounts(word).toSeq)
  }

  def parallelFoldSeq[A, R](seq: Seq[A], initial: R, parallelism: Int = 8)(reduceR: (R, R) => R)(foldR: (R, A) => R) : Future[R] = {
    @tailrec
    def foldSeqOffset(stream: Seq[A], state: R) : R = {
      stream match {
        case Nil => state
        case x +: xs => foldSeqOffset(xs.drop(parallelism - 1), foldR(state, x))
      }
    }

    Future sequence {
      for {
        offset <- 0 until parallelism
      } yield Future(foldSeqOffset(seq.drop(offset), initial))
    } map (_.reduce(reduceR))
  }

  def foldSeq[A, R](seq: Seq[A], initial: R)(foldR: (R, A) => R) : R = {
    @tailrec
    def foldSeqRec(stream: Seq[A], state: R) : R = {
      stream match {
        case Nil => state
        case x +: xs => foldSeqRec(xs, foldR(state, x))
      }
    }

    foldSeqRec(seq, initial)
  }

  def timeFunction[T](title: String, func: => T) : T = {
    val start = LocalDateTime.now()
    val res = func
    val stop = LocalDateTime.now()
    val dur = time.Duration.between(start, stop)
    println(s"$title took: ${java.text.NumberFormat.getIntegerInstance.format(dur.toMillis)} ms")
    res
  }

  private def findWords = foldSeq(dictionary, Seq.empty[String]) { (res, word) =>
    if (lookup(word)) res :+ word
    else res
  }

  private def findWordsPar = parallelFoldSeq(dictionary, Seq.empty[String])(_ ++ _) { (state, word) =>
    if (lookup(word)) state :+ word
    else state
  }

  def main(args: Array[String]) : Unit = {
    println(s"""Searching dictionary of ${dictionary.size} words for all anagrams of "$sourceText"...""")

    val wordSeq = timeFunction("Single-Threaded Search", findWords)
    println(s"Found ${wordSeq.size} words in all permutations")

    val words = timeFunction("Multi-Threaded Search", Await.result(findWordsPar, Duration.Inf))
    println(s"Found ${words.size} words in all permutations")

    println("First 25 found words: ")
    words take 25 foreach(word => println(s"\t$word"))
  }
}
