import util.input

import scala.language.postfixOps

private lazy val lines = input("Day09.txt")

private def part01(): Unit =
  val result = lines.map(line => ("-\\d+|\\d+".r findAllIn line toList).map(_.toInt))
    .map(seq => seq.last + calc(seq)).sum

  println("Part01")
  println(result)

private def part02(): Unit =
  val result = lines.map(line => ("-\\d+|\\d+".r findAllIn line toList).map(_.toInt).reverse)
    .map(seq => seq.last + calc(seq)).sum

  println("Part02")
  println(result)

private def calc(seq: List[Int]): Int =
  val numbers = seq.zipWithIndex.map((num, index) => grouped(seq, index))
    .foldLeft(List[Int]()) (accNext)

  if numbers.contains(0) && numbers.toSet.size == 1 then numbers.last else numbers.last + calc(numbers)

private def accNext(acc: List[Int], pair: List[Int]): List[Int] =
  if pair.length == 2
    then acc :+ pair.last - pair.head
  else acc

private def grouped(seq: List[Int], index: Int): List[Int] =
  if index + 1 < seq.length then List(seq.apply(index), seq.apply(index + 1)) else List(seq.apply(index))

@main def day09(): Unit =
  part01()
  part02()
