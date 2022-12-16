import java.io.File

class Sensor(val position: Pair<Int, Int>, val beacon : Pair<Int, Int>) {
  fun distanceToBeacon(): Int {
    return Math.abs(position.first - beacon.first) + Math.abs(position.second - beacon.second)
  }

}

fun parseInput(input: String): List<Sensor> {
  return input.lines().map { line ->
    Regex("""x=(-?\d+), y=(-?\d+).*x=(-?\d+), y=(-?\d+)""").find(line)?.destructured?.let { (x1, y1, x2, y2) ->
      Sensor(Pair(x1.toInt(), y1.toInt()), Pair(x2.toInt(), y2.toInt()))
    } ?: throw IllegalArgumentException("Invalid line: $line")
  }
}

fun mergeIntervals(intervals: List<Pair<Int, Int>>): List<Pair<Int, Int>> {
  return intervals.sortedBy { it.first }.fold(emptyList()) { acc, interval ->
    if (acc.isEmpty()) { listOf(interval) }
    else {
      if (acc.last().second >= interval.first) {
        acc.dropLast(1) + Pair(acc.last().first, Math.max(acc.last().second, interval.second))
      } else {
        acc + interval
      }
    }
  }
}

fun main() {
  val bufferedReader = File("input.txt").bufferedReader()
  val inputString = bufferedReader.use { it.readText() }
  val sensors = parseInput(inputString)
  val rowIdx = 2000000
  val closeSensors = sensors.filter { 
    it.position.second + it.distanceToBeacon() >= rowIdx && it.beacon.second - it.distanceToBeacon() <= rowIdx
  }
  val intervals = closeSensors.map {
    val x = it.position.first
    val y = it.position.second
    val dist = Math.abs(rowIdx - y)
    val x1 = x - (it.distanceToBeacon() - dist)
    val x2 = x + (it.distanceToBeacon() - dist) + 1
    Pair(x1, x2)
  }
  val mergedIntervals = mergeIntervals(intervals)
  println(mergedIntervals.fold(0) { acc, interval ->
    acc + (interval.second - interval.first) - 1
  })
}