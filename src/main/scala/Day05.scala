import util.input

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.{ListBuffer, Map}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.*
import scala.concurrent.{Await, Future}
import scala.util.control.Breaks.{break, breakable}

private lazy val lines = input("Day05.txt")

private val markers = Array("seed-to-soil map:", "soil-to-fertilizer map:", "fertilizer-to-water map:", "water-to-light map:",
  "light-to-temperature map:", "temperature-to-humidity map:", "humidity-to-location map:")

private def part01(): Unit =
  val seedsLine = lines.head
  val seeds = seedsLine.substring(seedsLine.indexOf(": ") + 2, seedsLine.length).split("\\s").map(s => s.toLong)

  val lookup = parse()

  val result = seeds.map(seed => findLocation(seed, lookup)).min

  println("Part01")
  println(result)

private def part02(): Unit =
  val seedsLine = lines.head
  val seeds = seedsLine.substring(seedsLine.indexOf(": ") + 2, seedsLine.length).split("\\s")
    .map(s => s.toLong).grouped(2).map(group => (group.head, group.head + group.last)).toList.sorted

  val lookup = parse()

  val futures = Future.sequence(seeds.map(tuple => tuple._1 until tuple._2).map(range => {
    Future(
      range.map(seed => findLocation(seed, lookup)).min
    )
  }))

  val result = Await.result(futures, 3.hours).min

  println("Part02")
  println(result)

private def findLocation(seed: Long, lookup: mutable.Map[String, ListBuffer[(Long, Long, Long)]]): Long =
  extract(seed, lookup, 0)

@tailrec
private def extract(source: Long, lookup: mutable.Map[String, ListBuffer[(Long, Long, Long)]], index: Int): Long =
  var value = source

  val values = lookup.apply(markers.apply(index))

  values.foreach(tuple =>
    breakable {
      // avoid overwriting
      if (value == source) {
        // (destination, source, range)
        if tuple._2 to (tuple._2 + tuple._3) contains source then value = tuple._1 + (source - tuple._2)

        if value != source then break()
      }
    }
  )

  if index + 1 < markers.length then extract(value, lookup, index + 1) else value

private def lineToTuple(line: String): (Long, Long, Long) =
  val split = line.split("\\s").map(s => s.toLong)

  (split(0), split(1), split(2))

private def parse(): mutable.Map[String, ListBuffer[(Long, Long, Long)]] =
  lazy val lookup = mutable.Map[String, ListBuffer[(Long, Long, Long)]]()

  var marker = markers.head

  lines.iterator.zipWithIndex.filter((line, index) => index > 2 && line != "").foreach((line, index) => {
    val query = markers.indexOf(line)

    if query > -1 then
      marker = markers.apply(query)
    else
      val list = if lookup.contains(marker) then lookup.apply(marker) else ListBuffer()

      list += lineToTuple(line)

      lookup += (marker -> list)
  })

  lookup

@main def day05(): Unit =
  // part01()
  part02()

