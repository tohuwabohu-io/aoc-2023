import util.input

private lazy val lines = input("Day02.txt")

private val maxValueMap = Map(
  "red" -> 12,
  "green" -> 13,
  "blue" -> 14
)

private def part01(): Unit =
  val result = lines.filter(line => filterColor("red", line))
    .filter(line => filterColor("blue", line))
    .filter(line => filterColor("green", line))
    .map(line => ("Game \\d+".r findFirstIn line).get)
    .map(gameId => gameId.substring(gameId.indexOf(' '), gameId.length).trim.toInt)
    .sum

  println("Part 01")
  println(result)

private def part02(): Unit =
  val result = lines.map(line => {
    findMax("red", line) * findMax("blue", line) * findMax("green", line)
  }).sum

  println("Part 02")
  println(result)

private def filterColor(color: String, line: String): Boolean =
  val totalMatches = (s"\\d+ $color".r findAllIn line).toList
  val filteredMatches = totalMatches.map(m => m.substring(0, m.indexOf(' ')).toInt).filter(number => number <= maxValueMap(color))

  totalMatches.size == filteredMatches.size

private def findMax(color: String, line: String): Int =
  val totalMatches = (s"\\d+ $color".r findAllIn line).toList
  val filteredMatches = totalMatches.map(m => m.substring(0, m.indexOf(' ')).toInt).max

  filteredMatches

@main def day02(): Unit =
  part01()
  part02()