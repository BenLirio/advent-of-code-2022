import java.io.File

class Sensor(val position: Pair<Int, Int>, val beacon : Pair<Int, Int>) {
  val distanceToBeacon: Int = Math.abs(position.first - beacon.first) + Math.abs(position.second - beacon.second)
}

fun parseInput(input: String): List<Sensor> {
  return input.lines().map { line ->
    Regex("""x=(-?\d+), y=(-?\d+).*x=(-?\d+), y=(-?\d+)""").find(line)?.destructured?.let { (x1, y1, x2, y2) ->
      Sensor(Pair(x1.toInt(), y1.toInt()), Pair(x2.toInt(), y2.toInt()))
    } ?: throw IllegalArgumentException("Invalid line: $line")
  }
}

fun main() {
  val bufferedReader = File("input.txt").bufferedReader()
  val inputString = bufferedReader.use { it.readText() }
  val sensors = parseInput(inputString)
  var y = 0
  while (y < 4000000) {
  // while (y < 20) {
    var x = 0
    while (x < 4000000) {
    // while (x < 20) {
      var c = 1
      var allTooFar = true
      for (sensor in sensors) {
        val dist = Math.abs(sensor.position.first - x) + Math.abs(sensor.position.second - y)
        if (dist <= sensor.distanceToBeacon) {
          allTooFar = false
        }
        c = Math.max(c, sensor.distanceToBeacon - dist + 1)
      }
      if (allTooFar) {
        println(x)
        println(y)
      }
      x += c
    }
    y += 1
  }
}