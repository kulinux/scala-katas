package queens

import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric._

/*
import cats._
import cats.data._
import cats.implicits._
*/


case class Queen(x: Queen.ChessBox, y: Queen.ChessBox)
case class Jugada(jugada: List[Queen])

object Queen {
  type ChessBoxRefined = Interval.Closed[0, 8]  
  type ChessBox = Int Refined ChessBoxRefined

  def diagonal(q1: Queen, q2: Queen): Boolean = 
    return Math.abs(q1.x.value - q1.y.value) == Math.abs(q2.x.value - q2.y.value)

  def check(q1: Queen, q2: Queen): Boolean = diagonal(q1, q2)
  
  def check(jugada: Jugada): Boolean = {
    val tuples:List[(Queen, Queen)] = for {
      q1 <- jugada.jugada
      q2 <- jugada.jugada.filter(_ != q1)
    } yield (q1, q2)

    tuples.foldLeft(false)((b, qs) => b match {
      case true => true
      case false => check(qs._1, qs._2)
      })
  }
}

object Main extends App {
}
