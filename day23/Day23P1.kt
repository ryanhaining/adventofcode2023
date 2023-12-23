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

private sealed interface Tile

private object Blocked : Tile {
  override fun toString() = "#"
}

private open class Path(val directions: List<Direction>) : Tile {
  var used = false
}

private class Flat : Path(Direction.entries) {
  override fun toString() = "."
}

private class Slope(direction: Direction) : Path(listOf(direction)) {
  override fun toString() = directions[0].toString().slice(0..0)
}

private typealias Grid = List<List<Tile>>

private fun dfs(grid: Grid, p: Point): Int? {
  if (p.row == grid.size-1 && p.col == grid.size-2) {
    return 0 // reached destination
  }
  if (p.row < 0) {
    return null // trying to go off the top
  }
  val current = grid[p.row][p.col]
  val result = when (current) {
    is Blocked -> null
    is Path -> {
      if (current.used) {
        null
      } else {
        current.used = true
        val max = current.directions
          .mapNotNull { d -> dfs(grid, d.adjust(p)) }
          .maxOrNull()
        current.used = false
        max
      }
    }
  }
  return result?.plus(1)
}


fun main() {
  val grid = generateSequence(::readLine).map { line ->
    line.map { c ->
      when (c) {
        '#' -> Blocked
        '.' -> Flat()
        '>' -> Slope(Direction.RIGHT)
        '<' -> Slope(Direction.LEFT)
        '^' -> Slope(Direction.UP)
        'v' -> Slope(Direction.DOWN)
        else -> error("Usupported character: $c")
      }
    }
  }.toList()
  println(dfs(grid, Point(0, 1)))
}
