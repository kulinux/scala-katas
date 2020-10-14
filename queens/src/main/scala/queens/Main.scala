package queens

import scala.collection.mutable



case class Queen(x: Int, y: Int)
case class Jugada(jugada: List[Queen])

object Queen {
  val queenPositionInBoard = for {
    x <- 0 to 8
    y <- 0 to 8
  } yield Queen(x, y)

  var totalTimeCheck = 0L
  def check(q1: Queen, q2: Queen): Boolean = {
    val st = System.currentTimeMillis()
    val res = q1.x == q2.x || q1.y == q2.y ||
        Math.abs(q1.x - q1.y) == Math.abs(q2.x - q2.y)
    val et = System.currentTimeMillis()
    totalTimeCheck = totalTimeCheck + (et - st)
    res
  }

  val allCheck = calculateAllCheck()

  def checkCache(q1: Queen, q2: Queen) = {
    allCheck.get((q1, q2)).getOrElse(false)
  }


  def calculateAllCheck() : mutable.Map[Tuple2[Queen, Queen], Boolean] = {
    var totalTimeCache = 0L
    val st = System.currentTimeMillis()
    val all: mutable.Map[Tuple2[Queen, Queen], Boolean]  = new mutable.HashMap()
    val queenPositionInBoardLocal = for {
      x <- 0 to 8
      y <- 0 to 8
    } yield Queen(x, y)

    val allPairs = for {
      q1 <- queenPositionInBoardLocal
      q2 <- queenPositionInBoardLocal
    } yield (q1, q2)

    for(pair <- allPairs) {
      all.put(pair, check(pair._1, pair._2))
    }
    val et = System.currentTimeMillis()
    totalTimeCache = totalTimeCache + (et - st)
    println("Time Cache " + totalTimeCache)
    all
  }

  
  var totalTimeLoop = 0L
  def check(jugada: Jugada): Boolean = {
    if(jugada.jugada.length == 1) return false

    val st = System.currentTimeMillis()
    val withIndex = jugada.jugada.zipWithIndex
    for(q1 <- withIndex) {
      for(q2 <- withIndex) {
        if(q1._2 != q2._2 && checkCache(q1._1, q2._1)) {
          val et = System.currentTimeMillis()
          totalTimeLoop = totalTimeLoop + (et - st)
          return true
        }
      }
    }
    val et = System.currentTimeMillis()
    totalTimeLoop = totalTimeLoop + (et - st)
    return false
  }


  def queensInBoard(colocadas: Jugada): List[Jugada] = {
    val x = colocadas.jugada.map(_.x).distinct
    val y = colocadas.jugada.map(_.y).distinct
    val diagonal = colocadas.jugada.map(d => Math.abs(d.x - d.y)).distinct
    val resFilter = queenPositionInBoard
      .filter(q => x.contains(q.x) == false && y.contains(q.y) == false && diagonal.contains(Math.abs(q.x - q.y)) == false )
      .map(q => Jugada(colocadas.jugada ++ List(q)))

    resFilter.toList
  }

  def exhaustivo(): List[Jugada] = {
    for {
      q1 <- queensInBoard(Jugada(List()))
      q2 <- queensInBoard(q1)
      q3 <- queensInBoard(q2)
      q4 <- queensInBoard(q3)
      q8 <- queensInBoard(q4) if(check(q8) == false)
    } yield q8

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
    } yield q8
    */
    
  }
}

object Main extends App {
}
