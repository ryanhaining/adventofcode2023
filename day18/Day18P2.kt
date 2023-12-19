import kotlin.math.abs

private data class Point(val row: Long, val col: Long)

private enum class Direction {
  UP {
    override fun adjust(point: Point, distance: Long) = point.copy(row=point.row-distance)
  },
  DOWN {
    override fun adjust(point: Point, distance: Long) = point.copy(row=point.row+distance)
  },
  LEFT {
    override fun adjust(point: Point, distance: Long) = point.copy(col=point.col-distance)
  },
  RIGHT {
    override fun adjust(point: Point, distance: Long) = point.copy(col=point.col+distance)
  };
  abstract fun adjust(point: Point, distance: Long): Point
  companion object {
    fun from(c: Char): Direction = when(c) {
      '0' -> RIGHT
      '1' -> DOWN
      '2' -> LEFT
      '3' -> UP
      else -> error("unsupported char: $c")
    }
  }
}


fun main() {
  val points = mutableListOf<Point>(Point(0, 0))
  var length = 0L
  generateSequence(::readLine).forEach { line->
    val encoded = line.split("(#")[1].trimEnd(')')
    val direction = Direction.from(encoded.last())
    val distance = encoded.slice(0..<encoded.length-1).toLong(radix=16)
    length += distance
    points.add(direction.adjust(points.last(), distance))
  }
  points.removeLast()

  // shoelace formula
  val part1 = points.zipWithNext().map {(p1, p2) -> p1.row * p2.col}.sum() + points.last().row*points.first().col
  val part2 = points.zipWithNext().map {(p1, p2) -> p1.col * p2.row}.sum() + points.last().col*points.first().row
  // Only the half the border is counted, you can think of the points as being the top-left
  // corner of the hole. This means we need to add half the perimeter, then an additional
  // +1 for the 4 outer corners that don't have corresponding inner corners.
  val area = (abs(part1-part2)/2) + (length/2) + 1
  println(area)
}
