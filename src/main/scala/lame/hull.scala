package lame

import scala.{ specialized => spec }

import spire.algebra._
import spire.math._
import spire.implicits._
import spire.syntax._

object GrahamScan {
  def turn[@spec(Int, Double) R: Ring](p: (R, R), q: (R, R), r: (R, R)): R =
    (q._1 - p._1) * (r._2 - p._2) - (r._1 - p._1) * (q._2 - p._2)

  def hull[@spec(Int, Double) R: Ring: Order](unsorted: Set[(R, R)]): Seq[(R, R)] = {
    def ccw(p: (R, R), q: (R, R), r: (R, R)): Boolean =
      turn(p, q, r) > Ring[R].zero

    def envelope(ps: Seq[(R, R)]) = ps.foldLeft(Nil: List[(R, R)]) {
      case (q :: p :: hull, r) =>
        r :: (if (!ccw(p, q, r)) p :: hull else q :: p :: hull)
      case (hull, r) => r :: hull
    }

    val sorted = unsorted.toList.qsorted
    val lower = envelope(sorted)
    val upper = envelope(sorted.reverse)
    lower ++ upper.tail.init
  }

  def genPoints[R](n: Int)(f: => R): Set[(R, R)] =
    List.fill(n)((f, f)).toSet
}
