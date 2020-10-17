package klondike

import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.Interval
import eu.timepit.refined.collection.Size

sealed trait Colour
object Red extends Colour
object Black extends Colour

sealed trait Suit {
   val colour: Colour
}
object Diamond extends Suit {
    override val colour = Red
}
object Pike extends Suit {
    override val colour = Black
}
object Heart extends Suit {
    override val colour = Red
}
object Club extends Suit {
    override val colour = Black
}

object RefinedTypes {
    type CardNumber = Int Refined CardNumberRestrictions
    type CardNumberRestrictions = Interval.Closed[1, 13]

    type Pile = List[Option[PileOfCards]] Refined PileRestrictions
    type PileRestrictions = Size[7]

    type FoundationPile = List[Option[PileOfCards]] Refined PileRestrictions
    type FoundationPileRestrictions = Size[4]
}

case class Card(number: RefinedTypes.CardNumber, suit: Suit)

class PileOfCards(down: List[Card], up: List[Card]) {
    def turnUp(): Option[PileOfCards] = {
        down match {
            case Nil => Option.empty
            case head :: tail => Some(new PileOfCards(tail, head :: up))
        }
    }
    def takeFirst(): (Option[Card], PileOfCards) = {
        up match {
            case Nil => (Option.empty, this)
            case head :: tail => (Some(head), new PileOfCards(down, tail))
        }
    }
    def add(card: Card): PileOfCards = new PileOfCards(down, card :: up)
}

case class Tableau(
    stock: PileOfCards,
    discardPile: PileOfCards,
    piles: RefinedTypes.Pile,
    foundationPiles: RefinedTypes.FoundationPile

)

object PileOfCards {
    def moveCard(from: PileOfCards, to: PileOfCards):
        (PileOfCards, PileOfCards) = {
            from.takeFirst() match {
                case (Some(card), pile) => (pile, to.add(card)) 
                case (None, pile) => (pile, to)
            }
    }
}