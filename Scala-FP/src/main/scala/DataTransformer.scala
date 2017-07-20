import java.io.{BufferedReader, Closeable, File, PrintWriter}

import scala.concurrent.Future
import scala.io.Source
import scala.util.Try
import scala.util.matching.Regex
import scala.concurrent.ExecutionContext.Implicits._

object DataTransformer {

  sealed trait LogLineType
  case object FirstLineType extends LogLineType
  case object SecondLineType extends LogLineType
  case object ThirdLineType extends LogLineType

  case class LogLine(logDateTime: String, authType: String, internalPath: String, mDate: String,
                      mTime:String, baseIp: String, version: String, idNo: String, pt: String, protocol: String,
                      cIp: String, sIp: String, atck: String, response: String, ckt: String, src: String, msg: String,
                      csPort: Int = -1, scPort: Int = -1, typeNo: Int = -1, lineCode: Int = -1, number: String = "", lineType: LogLineType) {

    final override def toString: String = {
      "\"" + logDateTime + "\"\t" + authType + "\t" + internalPath + "\t" + mDate + "\t" + mTime  + "\t" + baseIp + "\t" + version + "\t" +idNo + "\t" +
        pt + "\t" + protocol + "\t" + cIp + "\t" + sIp  + "\t" + atck + "\t" + response  + "\t" + ckt + "\t" + src + "\t\"" + msg + "\"\t" +
        csPort + "\t" +scPort + "\t" + typeNo + "\t" +lineCode + "\t" + number + "\n"
    }
  }

  case class Results(fileName: String, firstMatches: Int = 0, secondMatches: Int = 0, thirdMatches: Int = 0, unmatched: Int = 0)

  // Base regex string
  private val regexPatternBase = """(\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2})\t([^\t]+)\t([^\s]+)\t(\w{3} \d{2}) (\d{2}:\d{2}:\d{2}) (\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}) ([\w\d]+-[\w\d]+): id=(\d+) pt=(\w+-\w+) prot=(\w+) cip=(\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3})"""

  // Line match regexes
  private val firstRegex = { regexPatternBase + """ cprt=(\d+) sip=(\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}) sprt=(\d+) atck=(\w+-\d+) disp=(\bmonitor\b|\bmitigate\b) ckt=(\d+) src=(\bextern\b|\bintern\b) msg="([^\"]+)"(\b num=(\d+)\b)?""" }.r
  private val secondRegex = { regexPatternBase + """ sip=(\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}) type=(\d+) code=(\d+) atck=(\w+-\d+) disp=(\bmonitor\b|\bmitigate\b) ckt=(\d+) src=(\bextern\b|\bintern\b) msg="([^\"]+)"""" }.r
  private val thirdRegex = { regexPatternBase + """ sip=(\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}) atck=(\w+-\d+) disp=(\bmonitor\b|\bmitigate\b) ckt=(\d+) src=(\bextern\b|\bintern\b) msg=\"([^\"]+)\"( num=(\d+))?""" }.r

  @inline private def using[R <: Closeable, T](stream: R)(f: R => T): T = try {
    f(stream)
  } finally {
    stream.close()
  }

  @inline private def iterateMatchingFiles(path: String, fileNameMatch: Regex) : Seq[File] = {
    new File(path)
      .listFiles()
      .filter(file => fileNameMatch.findFirstIn(file.getName).isDefined)
      .toVector
  }

  @inline private def makeOutputFilename(sourceFile: File, postfix: String) =
    (sourceFile.getName.split(".").toSeq match {
      case name :: tail => name :: postfix :: tail
    }).mkString("")

  @inline private def makeOutputFile(sourceFile: File, postfix: String) =
    new File(sourceFile.getParentFile, makeOutputFilename(sourceFile, postfix))

  private def fileToStream(file: File) : Stream[String] = {
    def readFileRec(reader: BufferedReader) : Stream[String] = {
      reader.readLine() match {
        case null =>
          reader.close()
          Stream.empty
        case line => line #:: readFileRec(reader) // Sugar for: Stream.cons(line, readFileRec(reader))
      }
    }
    readFileRec(Source.fromFile(file).bufferedReader())
  }

  private def fileStreamToLogLines(fileStream: Stream[String]) : Stream[Either[LogLine, String]] = {
    fileStream map {
      case firstRegex(logDateTime, authType, internalPath, mDate, mTime, baseIp, version, idNo, pt, protocol, cIp, cprt, sip, sprt, atck, resp, ckt, src, msg, num) =>
        Left(LogLine(logDateTime, authType, internalPath, mDate, mTime, baseIp, version, idNo, pt, protocol, cIp, sip, atck, resp, ckt, src, msg, cprt.toInt, sprt.toInt, -1, -1, num, FirstLineType))
      case secondRegex(logDateTime, authType, internalPath, mDate, mTime, baseIp, version, idNo, pt, protocol, cIp, sip, typeNo, code, atck, resp, ckt, src, msg) =>
        Left(LogLine(logDateTime, authType, internalPath, mDate, mTime, baseIp, version, idNo, pt, protocol, cIp, sip, atck, resp, ckt, src, msg, -1, -1, typeNo.toInt, code.toInt, "", SecondLineType))
      case thirdRegex(logDateTime, authType, internalPath, mDate, mTime, baseIp, version, idNo, pt, protocol, cIp, sip, atck, resp, ckt, src, msg, num) =>
        Left(LogLine(logDateTime, authType, internalPath, mDate, mTime, baseIp, version, idNo, pt, protocol, cIp, sip, atck, resp, ckt, src, msg, -1, -1, -1, -1, num, ThirdLineType))
      case unmatchedLine => Right(unmatchedLine)
    }
  }

  private def processFileInMonad[M[_]](file: File, applyM: Results => M[Results])(logLineWriter: LogLine => Unit, unmatchedLineWriter: String => Unit) : M[Results] = {
    def foldOverFileLines(stream: Stream[Either[LogLine, String]]) = applyM {
      stream.foldLeft(Results(file.getName)) { case (res, currentLine) =>
        currentLine match {
          case Left(ll) =>
            logLineWriter(ll)
            ll.lineType match {
              case FirstLineType => res.copy(firstMatches = res.firstMatches + 1)
              case SecondLineType => res.copy(secondMatches = res.secondMatches + 1)
              case ThirdLineType => res.copy(thirdMatches = res.thirdMatches + 1)
          }
          case Right(ll) =>
            unmatchedLineWriter(ll)
            res.copy(unmatched = res.unmatched + 1)
        }
      }
    }

    foldOverFileLines(fileStreamToLogLines(fileToStream(file)))
  }

  @inline private def processFileTry(file: File)(logLineWriter: LogLine => Unit, unmatchedLineWriter: String => Unit) = processFileInMonad(file, Try(_))(logLineWriter, unmatchedLineWriter)
  @inline private def processFileFuture(file: File)(logLineWriter: LogLine => Unit, unmatchedLineWriter: String => Unit) = processFileInMonad(file, Future(_))(logLineWriter, unmatchedLineWriter)

  def processFilesInPath(inputPath: String, fileFilter: Regex, outputPath: String) : Try[Seq[Try[Results]]] = Try(iterateMatchingFiles(inputPath, fileFilter)) map { allFiles =>
    for (file <- allFiles) yield {
      using(new PrintWriter(makeOutputFile(file, "_processed"))) { outputWriter =>
        using(new PrintWriter(makeOutputFile(file, "_error"))) { errorWriter =>
          processFileTry(file)(logLine => outputWriter.write(logLine.toString), errorWriter.write)
        }
      }
    }
  }

  def processFilesInPathParallel(inputPath: String, fileFilter: Regex, outputPath: String) : Try[Seq[Future[Results]]] = Try(iterateMatchingFiles(inputPath, fileFilter)) map { allFiles =>
    for (file <- allFiles) yield {
      using(new PrintWriter(makeOutputFile(file, "_processed"))) { outputWriter =>
        using(new PrintWriter(makeOutputFile(file, "_error"))) { errorWriter =>
          processFileFuture(file)(logLine => outputWriter.write(logLine.toString), errorWriter.write)
        }
      }
    }
  }

}