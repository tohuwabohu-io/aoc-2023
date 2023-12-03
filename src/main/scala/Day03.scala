import util.input

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

private def findSymbols(startIndex: Int, endIndex: Int, line: String): Int =
  val start = if (startIndex < 0) 0 else startIndex
  val end = if (endIndex > line.length) line.length else endIndex

  ("[^\\d.\\r\\n]".r findAllIn line.substring(start, end)).size

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