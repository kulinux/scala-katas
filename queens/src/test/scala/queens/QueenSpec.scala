package queens

import org.scalatest.flatspec.AnyFlatSpec

//import cats._
//import cats.data._
//import cats.implicits._

//import eu.timepit.refined._
//import eu.timepit.refined.auto._

//import eu.timepit.refined.cats._
//import eu.timepit.refined.types.string._
import org.scalatest.matchers.should.Matchers

class QueenSpec extends AnyFlatSpec with Matchers {

    "Diagonal" should "inform check" in {
        Queen.check( Jugada(
            List( Queen(0, 0), Queen(0, 0) )
        )) shouldBe true

        Queen.check( Jugada(
            List( Queen(0, 0), Queen(1, 2) )
        )) shouldBe false

        Queen.check( Jugada(
            List( Queen(0, 0), Queen(0, 1), Queen(0, 3), Queen(4, 4) )
        )) shouldBe true
    }
}
class GeneradorSpec extends AnyFlatSpec with Matchers {
    "Generador" should "work" in {
        val res = Queen.queensInBoard(Jugada(List()))
        res.foreach(println)
    }
}

class QueenAppSpec extends AnyFlatSpec with Matchers {
    "Exhaustivo" should "work" in {
        val res = Queen.exhaustivo()
        println("Generado " + res.length)
        println("Time Loop " + Queen.totalTimeLoop)
        println("Time Check " + Queen.totalTimeCheck)
        //res.foreach(println)
    }
}
