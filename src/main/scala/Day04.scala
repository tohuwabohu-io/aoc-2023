import util.input

import scala.math.pow

private lazy val lines = input("Day04.txt")

private def part01(): Unit =
  val result = lines.map(line => {
    val parsed = parse(line)

    val intersection = List(parsed._1, parsed._2).reduce((a, b) => a intersect b)

    if intersection.length == 0 then 0 else pow(2, intersection.length - 1).toInt
  }).sum

  println("Part01")
  println(result)

private def part02(): Unit =
  val cardScore = collection.mutable.Map[Int, Int]()

  lines.map(line => parse(line)).foreach(parsed => cardScore += (parsed._3 -> 1))

  lines.foreach(line => {
    val parsed = parse(line)

    val cardsWon = List(parsed._1, parsed._2).reduce((a, b) => a intersect b).length
    val cardNumber = parsed._3
    val cardCount = cardScore.apply(cardNumber)

    if (cardsWon > 0) {
      val upperBounds = if (cardNumber + cardsWon) > cardScore.size then cardScore.size - 1 else cardNumber + cardsWon

      for (i <- cardNumber + 1 to upperBounds) {
        cardScore += (i -> (cardScore.apply(i) + cardCount))
      }
    }
  })

  val result = cardScore.values.sum

  println("Part02")
  println(result)

private def parse(line: String): (Array[Int], Array[Int], Int) =
  val split = line.replaceAll("\\t", " ").split('|')
  val leftSide = split.apply(0)
  val rightSide = split.apply(1)

  val winningNumbers = for (numberStr <- leftSide.substring(leftSide.indexOf(':') + 2, leftSide.length).split(' ').filter(s => s != "")) yield numberStr.trim.toInt
  val playedNumbers = for (numberStr <- rightSide.split(' ').filter(s => s != "")) yield numberStr.trim.toInt

  (winningNumbers, playedNumbers, ("\\d+".r findFirstIn leftSide).get.toInt)

@main def day04(): Unit =
  part01()
  part02()