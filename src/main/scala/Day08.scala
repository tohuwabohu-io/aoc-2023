import util.input

import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.postfixOps

private lazy val lines = input("Day08.txt")

private def part01(): Unit =
  val directions = lines.head

  val path = parsePath()

  var current = "AAA"
  var i = 0
  var cycles = 0

  while current != "ZZZ" do {
    val instruction = directions.charAt(i)

    current = if instruction == 'L' then path.apply(current)._1 else path.apply(current)._2

    i = if i < directions.length - 1 then i + 1 else 0
    cycles = cycles + 1
  }

  println("Part01")
  println(cycles)

private def part02(): Unit =
  val directions = lines.head
  val path = parsePath()

  val result = path.filter(a => a._1.endsWith("A")).keys.map(node => countSteps(node, directions, path)).toList.sorted
    .foldLeft(1L) ((witchcraft, steps) => conjure(witchcraft, steps))

  println("Part02")
  println(result)

private def conjure(a: Long, b: Int): Long = a * b / magic(a, b)

@tailrec
private def magic(a: Long, b: Long): Long = if b == 0 then a else magic(b, a % b)

private def parsePath(): mutable.LinkedHashMap[String, (String, String)] = lines.filter(_ contains "=").map(line => "[0-9A-Z]+".r findAllIn line toList)
  .map(node => node.head -> (node(1), node(2)))
  .foldLeft(mutable.LinkedHashMap[String, (String, String)]())((acc, node) => acc + (node._1 -> (node._2._1, node._2._2)))

private def countSteps(start: String, directions: String, path: mutable.LinkedHashMap[String, (String, String)]): Int =
  var i = 0
  var cycles = 0
  var current = start

  while !current.endsWith("Z") do {
    val instruction = directions.charAt(i)

    current = if instruction == 'L' then path.apply(current)._1 else path.apply(current)._2

    i = if i < directions.length - 1 then i + 1 else 0
    cycles = cycles + 1
  }

  cycles

@main def day08(): Unit =
  part01()
  part02()