package lame

import spire.algebra._
import spire.math._
import spire.implicits._
import spire.syntax._

final class Polynomial[R](val coef: Map[Int, R]) {
  override def toString = {
    coef.toList.sortBy(_._1).reverse map { case (i, c) =>
      s"$c * x ^ $i"
    } mkString " + "
  }

  def apply(x: R)(implicit R: Ring[R]): R =
    coef.map({ case (i, c) => c * (x ** i) }).foldLeft(R.zero)(_ + _)
}

object Polynomial {
  implicit def ring[R: Ring] = new PolynomialRing[R] {
    val R = Ring[R]
  }

  def apply[R: Ring](cs: (Int, R)*): Polynomial[R] =
    new Polynomial(cs.toMap)
}

trait PolynomialRing[R] extends Ring[Polynomial[R]] {
  implicit def R: Ring[R]

  def zero = new Polynomial(Map.empty)
  def one = new Polynomial(Map(0 -> R.one))
  def plus(x: Polynomial[R], y: Polynomial[R]): Polynomial[R] =
    new Polynomial(x.coef + y.coef)
  def negate(x: Polynomial[R]): Polynomial[R] =
    new Polynomial(x.coef mapValues R.negate)
  def times(x: Polynomial[R], y: Polynomial[R]): Polynomial[R] =
    x.coef.foldLeft(zero) { case (p, (i, c0)) =>
      plus(p, new Polynomial(y.coef map { case (j, c1) => (i + j) -> c0 * c1 }))
    }
}
