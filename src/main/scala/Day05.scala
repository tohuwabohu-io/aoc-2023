import util.input

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.util.control.Breaks.{break, breakable}
import concurrent.ExecutionContext.Implicits.global

private lazy val lines = input("Day05.txt")
private val markers = Array("seed-to-soil map:", "soil-to-fertilizer map:", "fertilizer-to-water map:", "water-to-light map:",
  "light-to-temperature map:", "temperature-to-humidity map:", "humidity-to-location map:")

private def part01(): Unit =
  val seedsLine = lines.head
  val seeds = seedsLine.substring(seedsLine.indexOf(": ") + 2, seedsLine.length).split("\\s").map(s => s.toLong)

  val result = seeds.map(seed => findLocation(seed, lines.iterator)).min

  println("Part01")
  println(result)

private def part02(): Unit =
  val seedsLine = lines.head
  val seeds = seedsLine.substring(seedsLine.indexOf(": ") + 2, seedsLine.length).split("\\s")
    .map(s => s.toLong).grouped(2).map(group => (group.head, group.head + group.last)).toList.sorted

  val merged = collapse(seeds);

  val futures = Future.sequence(merged.map(tuple => tuple._1 until tuple._2).map(range => {
    Future(
      range.map(seed => findLocation(seed, lines.iterator)).min
    )
  }))

  val result = Await.result(futures, 3.hours).min

  println("Part02")
  println(result)

private def findLocation(seed: Long, linesIterator: Iterator[String]): Long =
  linesIterator.next

  extract(seed, linesIterator, 1)

@tailrec
private def extract(source: Long, iterator: Iterator[String], index: Int): Long =
  var value = source

  val endTerm = if index < markers.length then markers.apply(index) else ""

  iterator.takeWhile(_!= endTerm).filter(s => s != "" && !markers.contains(s)).foreach(line => {
    breakable {
      // avoid overwriting
      if (value == source) {
        // (destination, source, range)
        val tuple = lineToTuple(line)
        if tuple._2 to (tuple._2 + tuple._3) contains source then value = tuple._1 + (source - tuple._2)

        if value != source then break()
      }
    }
  })

  if index < markers.length then extract(value, iterator, index + 1) else value

private def lineToTuple(line: String): (Long, Long, Long) =
  val split = line.split("\\s").map(s => s.toLong)

  (split(0), split(1), split(2))

@tailrec
private def collapse(rs: List[(Long, Long)], sep: List[(Long, Long)] = Nil): List[(Long, Long)] = rs match {
  case x :: y :: rest =>
    if (y._1 > x._2) collapse(y :: rest, x :: sep)
    else collapse((x._1, List(x._2, y._2).max) :: rest, sep)
  case _ =>
    (rs ::: sep).reverse
}

@tailrec
private def merge(ranges: List[(Long, Long)], acc: ListBuffer[(Long, Long)]): ListBuffer[(Long, Long)] = ranges match {
  case a :: b :: rest if a._2 >= b._1 => merge((a._1, Math.max(a._2, b._2)) :: rest, acc)
  case a :: b :: rest                 => merge(b :: rest, acc += a )
  case a :: Nil                       => acc += a
  case _                              => acc
}

@main def day05(): Unit =
  // part01()
  part02()

