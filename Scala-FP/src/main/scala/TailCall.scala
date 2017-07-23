import scala.annotation.tailrec
import scala.language.implicitConversions

sealed trait TailCall[+R] {
  import TailCall._
  def run : R = trampoline(this)
}

object TailCall {

  def tail[R](thunk: => TailCall[R]) : TailCall[R] = Call(() => thunk)
  def foldTail[R](rhs: TailCall[R], lhs: TailCall[R])(foldR: (R, R) => R) : TailCall[R] = Monoid(lhs, foldR, rhs)

  private final case class Return[+R](result: R) extends TailCall[R]
  private final case class Call[+R](fnc: () => TailCall[R]) extends TailCall[R]
  private final case class Monoid[R](lhs: TailCall[R], foldR: (R, R) => R, tailCall: TailCall[R]) extends TailCall[R]

  object Implicits {
    implicit def toReturn[R](result: R) : Return[R] = Return(result)
    implicit class FractionalExtensions[R](lhs: TailCall[R])(implicit fracOps: Fractional[R]) {
      def + (rhs: TailCall[R]) : TailCall[R] = Monoid(lhs, fracOps.plus, rhs)
      def - (rhs: TailCall[R]) : TailCall[R] = Monoid(lhs, fracOps.minus, rhs)
      def / (rhs: TailCall[R]) : TailCall[R] = Monoid(lhs, fracOps.div, rhs)
      def * (rhs: TailCall[R]) : TailCall[R] = Monoid(lhs, fracOps.times, rhs)
    }

    implicit class OptionExtensions[R, Opt <: Option[R]](lhs: TailCall[Opt]) {
      private def eitherOr(l: Opt, r: Opt) : Opt = l match {
        case res@Some(_) => res
        case _ => r
      }

      def orElse (rhs: => TailCall[Opt]) : TailCall[Opt] = Monoid(lhs, eitherOr, rhs)
    }
  }

  @tailrec
  private def trampoline[R](tc: TailCall[R]) : R = tc match {
    case Return(x) => x
    case Call(thunk) => trampoline(thunk())
    case Monoid(Return(r), foldR, Return(l)) => trampoline(Return(foldR(l, r)))
    case Monoid(rhs@Return(_), foldR, Call(lThunk)) => trampoline(Monoid(rhs, foldR, lThunk()))
    case Monoid(Call(rThunk), foldR, rhs@Return(_)) => trampoline(Monoid(rThunk(), foldR, rhs))
    case Monoid(Call(rThunk), foldR, Call(lThunk)) => trampoline(Monoid(rThunk(), foldR, lThunk()))
  }
}

abstract class TailRecMethod[A, R] extends (A => R) {
  final def apply(arg: A) : R = recursive(arg).run
  protected def recursive : A => TailCall[R]
}


sealed class BigDecimalCalculator[A](implicit opFrac: Fractional[BigDecimal]) {
  private type NumExpr = Expr[BigDecimal]
  private type Add = OpAdd[BigDecimal]
  private type Sub = OpSub[BigDecimal]
  private type Mul = OpMul[BigDecimal]
  private type Div = OpDiv[BigDecimal]


  private sealed class NumExprUnapply[Op <: NumExpr](f : Op => (NumExpr, NumExpr)) {
    def unapply(arg: Op) : Option[(NumExpr, NumExpr)] = Some(f(arg))
  }

  private object <+> extends NumExprUnapply[Add]({ case OpAdd(l, r) => (l, r)})
  private object <-> extends NumExprUnapply[Sub]({ case OpSub(l, r) => (l, r)})
  private object <*> extends NumExprUnapply[Mul]({ case OpMul(l, r) => (l, r)})
  private object </> extends NumExprUnapply[Div]({ case OpDiv(l, r) => (l, r)})

  def interpret(expr: NumExpr) : BigDecimal = {
    import TailCall.Implicits._

    val intRec = new TailRecMethod[NumExpr, BigDecimal] {
      protected val recursive: NumExpr => TailCall[BigDecimal] = {
        case Val(v) => v
        case l <+> r => recursive(l) + recursive(r)
        case l <-> r => recursive(l) - recursive(r)
        case l <*> r => recursive(l) * recursive(r)
        case l </> r => recursive(l) / recursive(r)
        case _ => throw new RuntimeException("BigDecimalCalculator does not support the '%' (modulus) operation.")
      }
    }

    intRec(expr)
  }

}