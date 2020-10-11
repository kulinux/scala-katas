package queens

import org.scalatest.flatspec.AnyFlatSpec

//import cats._
//import cats.data._
import cats.implicits._

import eu.timepit.refined._
import eu.timepit.refined.auto._

//import eu.timepit.refined.cats._
//import eu.timepit.refined.types.string._
import org.scalatest.matchers.should.Matchers

class QueenSpec extends AnyFlatSpec with Matchers {
    "Diagonal" should "work diagonal" in {
        Queen.diagonal(Queen(1, 1), Queen(2, 2)) shouldBe true
        Queen.diagonal(Queen(1, 2), Queen(2, 3))  shouldBe true
        Queen.diagonal(Queen(1, 2), Queen(5, 3))  shouldBe false
        Queen.diagonal(Queen(1, 2), Queen(6, 7)) shouldBe true
    }

    "Diagonal" should "inform check" in {
        Queen.check( Jugada(
            List( Queen(0, 0), Queen(0, 1), Queen(0, 3), Queen(0, 4) )
        )) shouldBe false

        Queen.check( Jugada(
            List( Queen(0, 0), Queen(0, 1), Queen(0, 3), Queen(4, 4) )
        )) shouldBe true
    }
}

class QueenAppSpec extends AnyFlatSpec with Matchers {
    "Exhaustivo" should "work" in {
        println("Encontre " + Queen.exhaustivo())
    }
}

class PruebasSpec extends AnyFlatSpec with Matchers {
    "Test" should "work" in {
        val q = for {
            x <- refineV[Queen.ChessBoxRefined](2)
            y <- refineV[Queen.ChessBoxRefined](4)
        } yield Queen(x, y)

        val q2 = (refineV[Queen.ChessBoxRefined](2), refineV[Queen.ChessBoxRefined](3)).mapN(Queen.apply)

        q.isRight shouldBe true
        q2.isRight shouldBe true
    }
}