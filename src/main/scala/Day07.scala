import util.input

import scala.language.postfixOps
import scala.math
import scala.util.control.Breaks.{break, breakable}

private lazy val lines = input("Day07.txt")
private var joker = false

private val handScores = Map(
  (5, 1) -> 6, // grande
  (4, 1) -> 5, // fours
  (3, 2) -> 4, // full house
  (3, 1) -> 3, // threes
  (2, 2) -> 2, // two pairs
  (2, 1) -> 1, // pair
  (1, 1) -> 0  // single
)

private val cardScores = Map(
  'A' -> 14,
  'K' -> 13,
  'Q' -> 12,
  'J' -> 11,
  'T' -> 10,
)

private def part01(): Unit =
  println("Part01")
  println(score())

private def part02(): Unit =
  joker = true

  println("Part02")
  println(score())

@main def day07(): Unit =
  // part01()
  part02()

private def score() =  lines.map(line => line.split("\\s"))
  .map(split => (split.head, split.last.toInt, scoreHand(split.head.sortWith(sortHand))))
  .sortWith(sortScore).map((hand, bid, score) => bid).reverse.zipWithIndex
  .map((bid, index) => (index + 1) * bid).sum

private def sortHand(a: Char, b: Char): Boolean = scoreCard(a) > scoreCard(b)
private def sortScore(a: (String, Int, Int), b: (String, Int, Int)): Boolean =
  if a._3 != b._3 then a._3 > b._3
  else
    var greater = false

    breakable {
      for (i <- 0 until a._1.length) {
        val score_a = scoreCard(a._1.charAt(i))
        val score_b = scoreCard(b._1.charAt(i))

        if score_a != score_b then {
          greater = score_a > score_b
          break()
        }
      }
    }

    greater

private def scoreHand(hand: String): Int =
  var matches = "([AKQJT2-9])\\1+".r findAllIn hand toList

  if joker && hand.contains('J') then matches = addJokers(matches, hand)

  if matches.length == 1 then
    handScores(matches.head.length, 1)
  else if matches.length == 2 then
    handScores(math.max(matches.head.length, matches.last.length), 2)
  else handScores((1, 1))

private def addJokers(matches: List[String], hand: String): List[String] =
  val highest = if matches.isEmpty then hand.apply(0) else matches.sortWith((a, b) => a.length > b.length).head.apply(0)
  val updated = StringBuilder(hand.replace('J', highest)).sortWith(sortHand)
  // not: 250541804
  // not: 250404255 <-- too high
  // not: 250368349 <-- too low
  // not: 249730980 <-- too low
  "([AKQJT2-9])\\1+".r findAllIn updated toList

private def scoreCard(card: Char): Int = if cardScores.contains(card) then
  if joker && card == 'J' then 0 else cardScores(card)
else card.asDigit
