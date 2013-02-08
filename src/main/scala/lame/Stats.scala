package lame

import spire.algebra._
import spire.math._
import spire.implicits._
import spire.syntax._


final case class Stats[A](sum: A, sumSq: A, count: Int) {
  def mean(implicit A: Field[A]) = sum / count
  def variance(implicit A: Field[A]) = sumSq / count - (sum / count) ** 2
  def stdDev(implicit A0: Field[A], A1: NRoot[A]) = variance.sqrt
}

object Stats {
  def apply[A: Field](a: A): Stats[A] = new Stats[A](a, a * a, 1)

  implicit def module[F: Field] = new StatsSpace[F]
}

final class StatsSpace[F: Field] extends Module[Stats[F], Int] {
  def scalar = Ring[Int]

  def zero: Stats[F] = new Stats(Field[F].zero, Field[F].zero, 0)
  def plus(x: Stats[F], y: Stats[F]): Stats[F] =
    new Stats(x.sum + y.sum, x.sumSq + y.sumSq, x.count + y.count)
  def negate(x: Stats[F]): Stats[F] =
    new Stats(-x.sum, -x.sumSq, -x.count)
  def timesl(s: Int, x: Stats[F]): Stats[F] =
    new Stats(s * x.sum, s * x.sumSq, s * x.count)
}


