import scala.annotation.tailrec
import scala.language.higherKinds

object Elves {

  val players = 410
  val stones = 7205900

  type L[A] = Vector[A]
  type S[A] = Vector[A]

  def main(args: Array[String]): Unit = {

    val scores = f(Vector(0, 1), 1, 2, Vector.fill(players+1)(0))
    println(scores.max)
  }

  @tailrec
  def f(state: L[Int], currentPosition: Int, currentMarble: Int, scores: S[Long]): S[Long] = {
    if (currentMarble > stones) return scores

    val player = ((currentMarble - 1) % players) + 1

    val (newState, newPos, scored) = if (currentMarble % 23 != 0) {
      (currentMarble +: state.rotate(currentPosition + 2) , 0, 0)
    }
    else {
      val (extraMarble, tail) = state.rotate(currentPosition - 7).pop()
      (tail, 0, currentMarble + extraMarble)
    }

    f(newState, newPos, currentMarble + 1, scores.updated(player, scored + scores(player)))
  }


  implicit class State[A](l: L[A]) {

    def rotate(pos: Int): L[A] = {
      val k = index(pos, l.size)
      val (front, back) = l.splitAt(k)
      back ++ front
    }

    def pop(): (A, L[A]) = {
      (l.head, l.tail)
    }

    private def index(i: Int, n: Int): Int = {
      (n + i) % n
    }

  }

}