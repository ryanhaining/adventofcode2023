import kotlin.math.abs

private data class Point(val row: Int, val col: Int)

private data class MutablePoint(var row: Int, var col: Int) {
  companion object {
    fun from(p: Point) = MutablePoint(p.row, p.col)
  }
}

private enum class Direction {
  UP {
    override fun adjust(point: Point, distance: Int) = point.copy(row=point.row-distance)
  },
  DOWN {
    override fun adjust(point: Point, distance: Int) = point.copy(row=point.row+distance)
  },
  LEFT {
    override fun adjust(point: Point, distance: Int) = point.copy(col=point.col-distance)
  },
  RIGHT {
    override fun adjust(point: Point, distance: Int) = point.copy(col=point.col+distance)
  };
  abstract fun adjust(point: Point, distance: Int): Point
  companion object {
    fun from(c: Char): Direction = when(c) {
      'U' -> UP
      'D' -> DOWN
      'L' -> LEFT
      'R' -> RIGHT
      else -> error("unsupported char: $c")
    }
  }
}


fun main() {
  val points = mutableListOf<Point>(Point(0, 0))
  generateSequence(::readLine).forEach { line->
    val (dirStr, distanceStr, _) = line.split(" ")
    require(dirStr.length == 1) { "got mult-char direction: $dirStr" }
    val direction = Direction.from(dirStr[0])
    val distance = distanceStr.toInt()
    points.add(direction.adjust(points.last(), distance))
  }
  points.removeLast()

  // Because the points represent the holes themselves, not the edges of the holes,
  // the area will be using the point as the outside edge on one side, but the
  // inside edge on the other. To compensate, we push the borders out when moving
  // down or left.
  // We need to make sure we are starting on the left side of the rectangle for this
  // specific version of the expansion to work
  val minCol = points.minBy {it.col }.col
  val leftSide = points.filter { it.col == minCol }
  val start = leftSide.minBy { it.row }
  val startIndex = points.indexOf(start)
  val adjustedPoints = (
      points.slice(startIndex..<points.size) +
      points.slice(0..<startIndex)
  ).map(MutablePoint::from)
  for ((p1, p2) in adjustedPoints.zipWithNext()) {
    check (p1.row == p2.row || p1.col == p2.col) {
      "Points are not in same row or same column: $p1, $p2"
    }
    if (p1.row < p2.row) {
      // moving down
      ++p1.col
      ++p2.col
    } else if (p2.col < p1.col) {
      // moving left
      ++p1.row
      ++p2.row
    } else {
      check (p1.row > p2.row || p2.col > p1.col) {
        "should be moving up or right but are not $p1 $p2"
      }
    }
  }

  // shoelace formula
  val part1 = adjustedPoints.zipWithNext().map {(p1, p2) -> p1.row * p2.col}.sum() + adjustedPoints.last().row*adjustedPoints.first().col
  val part2 = adjustedPoints.zipWithNext().map {(p1, p2) -> p1.col * p2.row}.sum() + adjustedPoints.last().col*adjustedPoints.first().row
  val area = abs(part1-part2)/2
  println(area)
}
