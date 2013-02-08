package lame

import spire.algebra._
import spire.math._
import spire.implicits._

object lines {
  type Line[A] = (A, A, A)

  def intersect[A: Field: Eq](a: Line[A], b: Line[A]): Option[(A, A)] = {
    val xn = (b._2 * a._2 - a._3 * b._2)
    val xd = (a._1 * b._2 - b._1 * a._2)
    val yn = (b._2 * a._1 - a._3 * b._1)

    if (xd === Field[A].zero)
      None
    else
      Some((xn / xd, -yn / xd))
  }
}
