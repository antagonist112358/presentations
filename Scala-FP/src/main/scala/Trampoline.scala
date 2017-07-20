import scala.annotation.tailrec

sealed trait Trampoline[A] {
  import Trampoline._

  def run : A = bounce(this)
}

object Trampoline {
  private final case class Return[A](result: A) extends Trampoline[A]
  private final case class Call[A](thunk: () => Trampoline[A]) extends Trampoline[A]

  def apply[A](result: A) : Trampoline[A] = Return(result)
  def apply[A](op: => Trampoline[A]) : Trampoline[A] = Call(() => op)

  @tailrec
  private def bounce[A](t: Trampoline[A]) : A = t match {
    case Call(thunk) => bounce(thunk())
    case Return(x) => x
  }
}
