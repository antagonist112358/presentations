import scala.util.Try


// Example of the IO A.D.T., with Monadic support
sealed trait IO[A] {
  import IO._

  def flatMap[B](f: A => IO[B]): IO[B] = Suspend(() => f(this.run))

  def map[B](f: A => B) : IO[B] = Return(() => f(this.run))

  private def run : A = this match {
    case Return(r) => r()
    case Suspend(s) => s().run
  }

  def runUnsafe : Either[Throwable, A] = Try(run).toEither
}

object IO {
  private final case class Return[A](r: () => A) extends IO[A]
  private final case class Suspend[A](s: () => IO[A]) extends IO[A]

  def apply[A](a: => A) : IO[A] = Return(() => a)

  def println(msg: String) : IO[Unit] = IO(Predef.println(msg))
}