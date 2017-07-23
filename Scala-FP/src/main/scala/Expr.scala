/**
  * Created by kdivincenzo on 7/22/2017.
  */
sealed trait Expr[+R]
case class Val[R](v: R) extends Expr[R]
case class OpAdd[R](lhs: Expr[R], rhs: Expr[R]) extends Expr[R]
case class OpSub[R](lhs: Expr[R], rhs: Expr[R]) extends Expr[R]
case class OpDiv[R](lhs: Expr[R], rhs: Expr[R]) extends Expr[R]
case class OpMul[R](lhs: Expr[R], rhs: Expr[R]) extends Expr[R]
case class OpMod[R](lhs: Expr[R], rhs: Expr[R]) extends Expr[R]


sealed class ExprCalculator[A](implicit opFrac: Fractional[A], ord: Ordering[A]) {
  import opFrac._
  type NumExpr = Expr[A]

  private case class TailCont(f: A => A) extends (A => A) {
    override def apply(v1: A): A = f(v1)
  }

  private object Point extends TailCont(x => x)

  private implicit class OpExtensions(lhs: A) {
    import ord._
    private final val typeZero = zero

    def %(rhs: A) : A = {
      def rec(x: A): A = x - rhs match {
        case v if v > typeZero => rec(v)
        case rem => rem + rhs
      }
      rec(lhs)
    }

  }

  // (1 + 5) + (6 + 3)
  // OpAdd( OpAdd(Val(1), Val(2)), OpAdd(Val(6), Val(3)) )
  private def interpretRec(expr: NumExpr, cont: A => A) : A = expr match {
    case Val(v) => cont(v)
    // Specializations
    case OpAdd(Val(l), Val(r)) => cont(l + r)
    case OpSub(Val(l), Val(r)) => cont(l - r)
    case OpMul(Val(l), Val(r)) => cont(l * r)
    case OpDiv(Val(l), Val(r)) => cont(l / r)
    case OpMod(Val(l), Val(r)) => cont(l % r)
    // Non-specialized cases
    case OpAdd(l, r) => interpretRec(l, TailCont(v => cont(v + interpretRec(r, Point))))
    case OpSub(l, r) => interpretRec(l, TailCont(v => cont(v - interpretRec(r, Point))))
    case OpDiv(l, r) => interpretRec(l, TailCont(v => cont(v / interpretRec(r, Point))))
    case OpMul(l, r) => interpretRec(l, TailCont(v => cont(v * interpretRec(r, Point))))
    case OpMod(l, r) => interpretRec(l, TailCont(v => cont(v % interpretRec(r, Point))))
  }

  def interpret(expr: NumExpr) : A = interpretRec(expr, Point)
}

object TestHarness {

  def BD(i: Int) : Val[BigDecimal] = Val(i)
  def BD(d: Double) : Val[BigDecimal] = Val(d)
  def Add(lhs: Expr[BigDecimal], rhs: Expr[BigDecimal]) = OpAdd(lhs, rhs)
  def Subt(lhs: Expr[BigDecimal], rhs: Expr[BigDecimal]) = OpSub(lhs, rhs)
  def Mult(lhs: Expr[BigDecimal], rhs: Expr[BigDecimal]) = OpMul(lhs, rhs)
  def Div(lhs: Expr[BigDecimal], rhs: Expr[BigDecimal]) = OpDiv(lhs, rhs)
  def Mod(lhs: Expr[BigDecimal], rhs: Expr[BigDecimal]) = OpMod(lhs, rhs)

  val calc = new ExprCalculator[BigDecimal]

  def displayResultOf(expr: Expr[BigDecimal]) : Unit = {
    val result = calc interpret expr
    println(s"Result: $result")
  }

  def main(args: Array[String]) : Unit = {
    displayResultOf {
      // (1 + 5) * (6 / 3)
      Mult(Add(BD(5), BD(1)), Div(BD(6), BD(3)))
    }

    displayResultOf {
      // ((15 / 3) * (1 / 2)) + (4 - 20 % 3)
      // 4.5
      Add(
        Mult(Div(BD(15), BD(3)), Div(BD(1), BD(2))),
        Subt(BD(4), Mod(BD(20), BD(3))))
    }

    // (1 / 13^6)
    displayResultOf {
      Div(BD(1),
        Mult(BD(13),
          Mult(BD(13),
            Mult(BD(13),
              Mult(BD(13),
                Mult(BD(13), BD(13))
              )
            )
          )
        )
      )
    }

  }
}