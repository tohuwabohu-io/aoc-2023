import util.input

private lazy val lines = input("Day01.txt")

def part01(): Unit =
  val sum = lines.map(line => ("\\d".r findAllIn line).toList)
    .map(numbers => s"${numbers.head}${numbers.last}")
    .map(number => number.toInt)
    .sum

  print("Part 01: ")
  println(sum)

def part02(): Unit =
  val numbersName = Map(
    "one" -> "o1e",
    "two" -> "t2o",
    "three" -> "t3e",
    "four" -> "f4r",
    "five" -> "f5e",
    "six" -> "s6x",
    "seven" -> "s7n",
    "eight" -> "e8t",
    "nine" -> "n9e"
  )

  val sum = lines.map(line => numbersName.foldLeft(line) ((a, b) => a.replace(b._1, b._2)) )
    .map(line => ("\\d".r findAllIn line).toList)
    .map(numbers => s"${numbers.head}${numbers.last}")
    .map(number => number.toInt)
    .sum

  print("Part 02: ")
  print(sum)

@main def day01(): Unit =
  println("Day 01")
  part01()
  part02()