import util.input

private lazy val lines = input("Day01.txt")

def part01(): Unit =
  println("Day 01")

  val sum = lines.map(line => ("\\d".r findAllIn line).toList)
    .map(numbers => numbers.head + numbers.last)
    .map(number => number.toInt)
    .sum

  print("Part 01: ")
  println(sum)

@main def day01(): Unit =
  part01()