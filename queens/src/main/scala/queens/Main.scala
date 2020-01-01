package queens

import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric._

import eu.timepit.refined._
import eu.timepit.refined.auto._

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

    if(jugada.jugada.length == 1) return false

    val tuples:List[(Queen, Queen)] = for {
      q1 <- jugada.jugada
      q2 <- jugada.jugada.filter(_.equals(q1) == false)
    } yield (q1, q2)

    if(tuples.length == 0) return true

    tuples.foldLeft(false)((b, qs) => b match {
      case true => true
      case false => check(qs._1, qs._2)
      })
  }

  def queensInBoard(colocadas: Jugada): List[Jugada] = {
    val res = for {
      x <- (0 to 8).map(refineV[Queen.ChessBoxRefined](_)).map(_.toOption.get)
      y <- (0 to 8).map(refineV[Queen.ChessBoxRefined](_)).map(_.toOption.get)
    } yield Queen(x, y)

    val resFilter = res.map(q => Jugada(colocadas.jugada ++ List(q))).filter(check(_) == false)

    resFilter.toList
  }

  def exhaustivo(): List[Jugada] = {
    for {
      q1 <- queensInBoard(Jugada(List()))
      q2 <- queensInBoard(q1) 
      q3 <- queensInBoard(q2)
      q4 <- queensInBoard(q3) if(check(q4) == false)
    } yield q4
    /*
    for {
      q1 <- queensInBoard(Jugada(List()))
      q2 <- queensInBoard(q1)
      q3 <- queensInBoard(q2)
      q4 <- queensInBoard(q3)
      q5 <- queensInBoard(q4)
      q6 <- queensInBoard(q5)
      q7 <- queensInBoard(q6)
      q8 <- queensInBoard(q7) if(check(q8) == false)
      _ = println(s"check $q8")
    } yield q8
    */
  }
}

object Main extends App {
}
