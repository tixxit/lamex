package lame

import spire.algebra._
import spire.implicits._

object groups {


  trait Catalogue[A] {
    def add[A](a: A): Unit
    def remove[A](a: A): Unit
  }
}
