package lame

import spire.algebra._
import spire.implicits._

import scala.util.Random

object GroupExample {
  sealed trait Event[+A] {
    def fold[B](f: A => B, g: A => B): B = this match {
      case Add(a) => f(a)
      case Remove(a) => g(a)
    }
  }
  case object Nop extends Event[Nothing]
  case class Add[A](item: A) extends Event[A]
  case class Remove[A](item: A) extends Event[A]

  trait Catalogue[A] {
    def reduceM[B: Monoid](f: Event[A] => B): B

    def reduce[B: Group](f: A => B): B = reduceM({
      case Add(a) => f(a)
      case Remove(a) => f(a).inverse
      case Nop => Group[B].id
    })
  }

  // We defined Stats as a Module, so we need to extract the additive group
  // for this example.
  implicit val StatsGroup = Group.additive[Stats[Double]]
  implicit val IntGroup = Group.additive[Int]

  case class Movie(title: String, length: Double)

  object Movies extends Catalogue[Movie] {

    // Probability that an event is an `Add[Movie]`.
    private val pAdd: Double = 0.65

    // Probability of a movie being put in the permanent collection.
    private val pPermanent: Double = 0.2

    // Average length of our movies.
    private val avgLength: Double = 100

    // Standard deviation of movie length.
    private val z: Double = 15

    // Total number of events to produce.
    private val events = 10000

    def stream: Iterator[Event[Movie]] = {
      // We use a fixed PRNG to keep our movie collection deterministic.
      val rng = new Random(42)
      import rng._

      val ps = Iterator.continually(nextDouble)
      ps.scanLeft((Nil, Nop): (List[Movie], Event[Movie])) {
        case ((movies, _), p) if p < pAdd || movies.isEmpty =>
          val m = Movie("The One and Only",
            math.max(0D, avgLength + nextGaussian * z))
          (m :: movies, Add(m))
        case ((movies0, _), _) =>
          val i = nextInt(movies0.size)
          val movies = ((movies0 take i) ++ (movies0 drop (i + 1))) filter { _ =>
            nextDouble > pPermanent
          }
          (movies, Remove(movies0(i)))
      } map (_._2) take events
    }

    def reduceM[B: Monoid](f: Event[Movie] => B): B =
      stream.foldLeft(Monoid[B].id) { (acc, ev) =>
        acc |+| f(ev)
      }
  }
}

object ModuleExample {
  sealed trait Event[+A] {
    def item: A
    def count: Int
  }
  case object Nop extends Event[Nothing] {
    def item = sys.error("!!!")
    def count = 0
  }
  case class Add[A](item: A, count: Int) extends Event[A]
  case class Remove[A](item: A, count: Int) extends Event[A]

  trait Catalogue[A] {
    def reduceM[B: Monoid](f: Event[A] => B): B

    def reduce[B](f: A => B)(implicit B: Module[B, Int]): B = reduceM({
      case Add(a, cnt) => cnt *: f(a)
      case Remove(a, cnt) => cnt *: -f(a)
      case Nop => B.zero
    })(B.additive)
  }

  sealed trait Product {
    def width: Double
    def height: Double
    def length: Double
    def volume: Double = width * height * length
  }
  case class Sprocket(radius: Double) extends Product {
    def width = radius * 2
    def height = 1D
    def length = radius * 2
  }
  case class Widget(width: Double, height: Double, length: Double) extends Product

  object Inventory extends Catalogue[Product] {

    // We use a fixed PRNG to keep our inventory deterministic.
    private val rng = new Random(2013)
    import rng._

    def stream: Iterator[Event[Product]] = {
      val rng = new Random(42)
      import rng._

      val ps = Iterator.continually(nextDouble)
      ps.scanLeft((Nil, Nop): (List[Event[Product]], Event[Product])) {
        case ((inventory, _), p) if p < 0.3 || inventory.isEmpty =>
          val event = if (nextBoolean) {
            Add(Sprocket(nextDouble * 100), nextInt(100) + 1)
          } else {
            Add(Widget(nextDouble * 100, nextDouble * 100, nextDouble * 100), nextInt(100) + 1)
          }
          (event :: inventory, event)

        case ((inventory, _), _) =>
          val i = nextInt(inventory.size)
          val p = inventory(i)
          val prefix = inventory take i
          val suffix = inventory drop (i + 1)
          val n = math.min(nextInt(3 * p.count / 2), p.count)
          val remove = Remove(p.item, n)
          if (n >= p.count) {
            (prefix ++ suffix, remove)
          } else {
            (Add(p.item, p.count - n) :: (prefix ++ suffix), remove)
          }
      } map (_._2) take 10000
    }

    def reduceM[B: Monoid](f: Event[Product] => B): B =
      stream.map(f).foldLeft(Monoid[B].id)(_ |+| _)
  }
}
