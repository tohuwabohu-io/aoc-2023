import util.input

import scala.util.control.Breaks.{break, breakable}

private lazy val lines = input("Day05.txt")
private val markers = Array("seed-to-soil map:", "soil-to-fertilizer map:", "fertilizer-to-water map:", "water-to-light map:",
  "light-to-temperature map:", "temperature-to-humidity map:", "humidity-to-location map:")

private def part01(): Unit =
  val seedsLine = lines.head
  val seeds = seedsLine.substring(seedsLine.indexOf(": ") + 2, seedsLine.length).split("\\s").map(s => s.toLong)

  val result = seeds.map(seed => findLocation(seed, lines.iterator)).min

  println("Part01")
  println(result)

private def findLocation(seed: Long, linesIterator: Iterator[String]): Long =
  linesIterator.next

  extract(
    extract(
      extract(
        extract(
          extract(
            extract(
              extract(seed, linesIterator, markers(1)
              ), linesIterator, markers(2)
            ), linesIterator, markers(3)
          ), linesIterator, markers(4)
        ), linesIterator, markers(5)
      ), linesIterator, markers(6)
    ), linesIterator, ""
  )

private def extract(source: Long, iterator: Iterator[String], endTerm: String): Long =
  var value = source

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

  value

private def lineToTuple(line: String): (Long, Long, Long) =
  val split = line.split("\\s").map(s => s.toLong)

  (split(0), split(1), split(2))

@main def day05(): Unit =
  part01()

