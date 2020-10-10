package queens

import org.scalatest.flatspec.AnyFlatSpec

import cats._
import cats.data._
import cats.implicits._

import eu.timepit.refined._
import eu.timepit.refined.auto._

import eu.timepit.refined.cats._
import eu.timepit.refined.types.string._
import org.scalatest.matchers.should.Matchers

class QueenSpec extends AnyFlatSpec with Matchers {
    "DiagonalUtil" should "work diagonal" in {
        Queen.diagonal(Queen(1, 1), Queen(2, 2)) shouldBe true
        Queen.diagonal(Queen(1, 2), Queen(2, 3))  shouldBe true
        Queen.diagonal(Queen(1, 2), Queen(5, 3))  shouldBe false
        Queen.diagonal(Queen(1, 2), Queen(6, 7)) shouldBe true
    }
}


class PruebasSpec extends AnyFlatSpec with Matchers {
    "Test" should "work" in {
        val q = for {
            x <- refineV[Queen.ChessBoxRefined](2)
            y <- refineV[Queen.ChessBoxRefined](4)
        } yield Queen(x, y)

        val q2 = (refineV[Queen.ChessBoxRefined](2), refineV[Queen.ChessBoxRefined](3)).mapN(Queen.apply)

        println(q)
        println(q2)
    }
}