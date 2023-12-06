import util.input

import scala.language.postfixOps

private lazy val lines = input("Day06.txt")

private def part01(): Unit =
  val times = "\\d+".r findAllIn lines.head toList
  val distances = "\\d+".r findAllIn lines.last toList

  val result = times.zip(distances)
    .map((time, distance) => calc(time.toInt, distance.toLong).count(_ > 1))
    .product

  println("Part01")
  println(result)

private def part02(): Unit =
  val times = "\\d+".r findAllIn lines.head mkString
  val distances = "\\d+".r findAllIn lines.last mkString

  val result = calc(times.toInt, distances.toLong).count(_ > 1)

  println("Part02")
  println(result)

private def calc(time: Int, distance: Long): IndexedSeq[Long] =
  for (i <- 1 until time) yield if i * (time - i).toLong > distance then time else 1

@main def day06(): Unit =
  part01()
  part02()
