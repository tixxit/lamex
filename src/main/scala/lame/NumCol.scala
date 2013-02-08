package lame

import spire.algebra._
import spire.math._
import spire.implicits._
import spire.syntax._

import java.lang.Double.isNaN


sealed trait NumCol extends PartialFunction[Int, Double] {
  def isDefinedAt(row: Int): Boolean
  def apply(row: Int): Double
}

case class DerivedNumCol(f: Int => Double)
extends NumCol {
  def isDefinedAt(row: Int): Boolean =
    row >= 0 && !isNaN(apply(row))
  def apply(row: Int): Double = f(row)
}

case class ArrayNumCol(values: Array[Double])
extends NumCol {
  def isDefinedAt(row: Int) =
    row >= 0 && row < values.length && !isNaN(values(row))
  def apply(row: Int) = values(row)
}

object NumCol {
  def apply(xs: Array[Double]): NumCol = new ArrayNumCol(xs)
  def apply(f: Int => Double): NumCol = new DerivedNumCol(f)

  private def op1(x: NumCol)(
      op: Double => Double): NumCol = x match {
    case DerivedNumCol(f) =>
      new DerivedNumCol(f andThen op)

    case ArrayNumCol(vals0) =>
      val vals = new Array[Double](vals0.length)
      cfor(0)(_ < vals.length, _ + 1) { row =>
        vals(row) = op(vals0(row))
      }
      new ArrayNumCol(vals)
  }

  private def op2(x: NumCol, y: NumCol)(
      op: (Double, Double) => Double): NumCol = (x, y) match {

    case (DerivedNumCol(f), DerivedNumCol(g)) =>
      new DerivedNumCol(row => op(f(row), g(row)))

    case (DerivedNumCol(f), ArrayNumCol(vals0)) =>
      val vals = new Array[Double](vals0.length)
      cfor(0)(_ < vals.length, _ + 1) { row =>
        vals(row) = op(f(row), vals0(row))
      }
      new ArrayNumCol(vals)

    case (ArrayNumCol(vals0), DerivedNumCol(f)) =>
      val vals = new Array[Double](vals0.length)
      cfor(0)(_ < vals.length, _ + 1) { row =>
        vals(row) = op(vals0(row), f(row))
      }
      new ArrayNumCol(vals)

    case (ArrayNumCol(vals0), ArrayNumCol(vals1)) =>
      val vals = new Array[Double](math.min(vals0.length, vals1.length))
      cfor(0)(_ < vals.length, _ + 1) { row =>
        vals(row) = op(vals0(row), vals1(row))
      }
      new ArrayNumCol(vals)
  }

  implicit object field extends Field[NumCol] with NRoot[NumCol] {
    def zero = new DerivedNumCol(_ => 0.0)
    def one = new DerivedNumCol(_ => 1.0)
    def plus(x: NumCol, y: NumCol): NumCol = op2(x, y)(_ + _)
    override def minus(x: NumCol, y: NumCol): NumCol = op2(x, y)(_ - _)
    def negate(x: NumCol): NumCol = op1(x)(-_)
    def times(x: NumCol, y: NumCol): NumCol = op2(x, y)(_ * _)
    def mod(x: NumCol, y: NumCol): NumCol = op2(x, y)(_ % _)
    def quot(x: NumCol, y: NumCol): NumCol = op2(x, y)(_ / _)
    def div(x: NumCol, y: NumCol): NumCol = op2(x, y)(_ * _)
    def ceil(x: NumCol): NumCol = op1(x)(math.ceil)
    def floor(x: NumCol): NumCol = op1(x)(math.floor)
    def round(x: NumCol): NumCol = op1(x)(f => math.round(f))
    def isWhole(x: NumCol): Boolean = false
    def nroot(x: NumCol, k: Int): NumCol = op1(x)(_ nroot k)
    def fpow(x: NumCol, y: NumCol): NumCol = op2(x, y)(math.pow)
    def log(x: NumCol): NumCol = op1(x)(math.log)
    def gcd(x: NumCol, y: NumCol): NumCol = op2(x, y)(Field[Double].gcd)
  }
}
