private data class Point(val row: Int, val col: Int)

private enum class Direction {
  UP {
    override fun adjust(point: Point) = point.copy(row=point.row-1)
  },
  DOWN {
    override fun adjust(point: Point) = point.copy(row=point.row+1)
  },
  LEFT {
    override fun adjust(point: Point) = point.copy(col=point.col-1)
  },
  RIGHT {
    override fun adjust(point: Point) = point.copy(col=point.col+1)
  };
  abstract fun adjust(point: Point): Point
}

private data class Seen(val steps: Int, val p: Point)

private val seen = mutableSetOf<Seen>()

private fun markSteps(p: Point, grid: List<MutableList<Char>>, steps: Int) {
  if (p.row < 0 || p.row >= grid.size
  || p.col < 0 || p.col >= grid[p.row].size
  || grid[p.row][p.col] == '#') {
    return
  }
  val seenData = Seen(steps, p)
  if (seenData in seen) {
    return
  }
  seen.add(seenData)
  require(grid[p.row][p.col] in "OS.")
  if (steps == 0) {
    grid[p.row][p.col] = 'O'
    return
  }
  for (d in Direction.entries) {
    markSteps(d.adjust(p), grid, steps-1)
  }
}

fun main() {
  val grid: List<MutableList<Char>> = generateSequence(::readLine).map {
    it.toMutableList()
  }.toList()

  val startRow = grid.withIndex().first { (_, line) -> 'S' in line }.index
  val startCol = grid[startRow].indexOf('S')
  val start = Point(startRow, startCol)

  markSteps(start, grid, 64)

  val total = grid.map { it.count{ it=='O' } }.sum()
  println(total)
}
