import util.input

import scala.collection.mutable.ListBuffer

private lazy val lines = input("Day03.txt")

private def part01(): Unit =
  val result = lines.map(line => batch(lines, line)).map(triplet => {
    val numberMatches = ("\\d+".r findAllIn triplet._2).matchData

    numberMatches.map(matchData => {
      val number = matchData.matched.toInt

      val indexStart = matchData.start - 1
      val indexEnd = matchData.end + 1

      val above = if triplet._1 != null then findSymbols(indexStart, indexEnd, triplet._1) else 0
      val adjacent = findSymbols(indexStart, indexEnd, triplet._2)
      val below = if triplet._3 != null then findSymbols(indexStart, indexEnd, triplet._3) else 0

      val sum = above + adjacent + below

      number * sum
    }).sum
  }).sum

  println("Part 01")
  println(result)

private def part02(): Unit =
  val result = lines.map(line => batch(lines, line)).map(triplet => {
    val symbolMatches = ("[^\\d.\\r\\n]".r findAllIn triplet._2).matchData

    symbolMatches.map(matchData => {
      val indexStart = matchData.start - 1
      val indexEnd = matchData.end + 1

      val above = if triplet._1 != null then findNumbers(indexStart, indexEnd, triplet._1) else List()
      val adjacent = findNumbers(indexStart, indexEnd, triplet._2)
      val below = if triplet._3 != null then findNumbers(indexStart, indexEnd, triplet._3) else List()

      val values = above ++ adjacent ++ below

      if values.length == 2 then {
        values.product
      } else 0
    }).sum
  }).sum

  println("Part 02")
  println(result)

private def findSymbols(startIndex: Int, endIndex: Int, line: String): Int =
  val start = if (startIndex < 0) 0 else startIndex
  val end = if (endIndex > line.length) line.length else endIndex

  ("[^\\d.\\r\\n]".r findAllIn line.substring(start, end)).size

private def findNumbers(startIndex: Int, endIndex: Int, line: String): List[Int] =
  val start = if (startIndex < 0) 0 else startIndex
  val end = if (endIndex > line.length) line.length else endIndex

  val substring = line.substring(start, end)

  var numbers = ListBuffer[Int]()

  if (!substring.startsWith(".") && !substring.endsWith(".") && !substring.matches("\\d+")) {
    numbers += ("\\d+".r findFirstIn line.substring(start + 1, line.length)).get.toInt
    numbers += ("\\d+".r findFirstIn line.substring(0, end - 1).reverse).get.reverse.toInt
  } else {
    val it = ("\\d+".r findAllIn substring).matchData

    while (it.hasNext) {
      val next = it.next()

      if (substring.startsWith(".")) {
        // look right
        numbers += ("\\d+".r findFirstIn line.substring(start, line.length)).get.toInt
      } else if (substring.endsWith(".")) {
        // look left
        numbers += ("\\d+".r findFirstIn line.substring(0, end).reverse).get.reverse.toInt
      } else {
        numbers += next.matched.toInt
      }
    }
  }

  numbers.result()

private def batch(lines: LazyList[String], line: String) =
  (getPreviousLine(lines, line), line, getNextLine(lines, line))

private def getPreviousLine(lines: LazyList[String], line: String): String =
  val previousIndex = lines.indexOf(line) - 1

  if previousIndex < 0 then null else lines.apply(previousIndex)

private def getNextLine(lines: LazyList[String], line: String): String =
  val nextIndex = lines.indexOf(line) + 1

  if nextIndex >= lines.length then null else lines.apply(nextIndex)

@main def day03(): Unit =
  part01()
  part02()