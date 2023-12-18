import java.util.TreeSet

const val MIN_STRAIGHT = 4
const val MAX_STRAIGHT = 10

private typealias Grid = List<List<Int>>
private typealias EdgeGrid = List<List<List<Edge>>>

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

private val Direction.opposite get(): Direction = when(this) {
  Direction.DOWN -> Direction.UP
  Direction.UP -> Direction.DOWN
  Direction.LEFT -> Direction.RIGHT
  Direction.RIGHT -> Direction.LEFT
}

private data class Edge(
    val dest: Point,
    val direction: Direction,
    val dirCount: Int,
) {
  // top left gets distance 0
  // distance is excluded from .equals() which is fine
  var distance: Int = if (dest.row == 0 && dest.col == 0) 0 else Int.MAX_VALUE
  override fun toString(): String =
    "Edge(dest=$dest, ${direction}x$dirCount, distance=$distance)"
  // prevents TreeSet from deduping when range is equal
  val uniqueId: Int = ++idTracker
  companion object {
    var idTracker = 0
  }
}

private fun edgesFrom(grid: Grid, src: Point): List<Edge> =
  (1..MAX_STRAIGHT).flatMap { dirCount ->
    Direction.entries.mapNotNull { dir ->
      val dest = dir.adjust(src)
      if (isInBounds(grid, dest)) Edge(dest, dir, dirCount) else null 
    }
  }

private fun allEdgesFrom(grid: Grid): EdgeGrid =
  grid.withIndex().map { (rowIndex, row) ->
    row.withIndex().map { (colIndex, _) ->
      edgesFrom(grid, Point(rowIndex, colIndex))
    }
  }

private fun isInBounds(grid: Grid, point: Point): Boolean =
  point.row >= 0 && point.col >= 0 && point.row < grid.size && point.col < grid[point.row].size

private fun availableEdgesFrom(edgeIn: Edge, edgesFrom: EdgeGrid): List<Edge> =
  edgesFrom[edgeIn.dest.row][edgeIn.dest.col].filter {
    it.direction != edgeIn.direction.opposite // can't turn around
    && ((it.direction != edgeIn.direction  && edgeIn.dirCount >= MIN_STRAIGHT && it.dirCount == 1) // turn with dirCount of 1
    || (it.direction == edgeIn.direction && it.dirCount == edgeIn.dirCount+1)) // straight with dirCount+1
  }

private fun runDijkstra(heap: TreeSet<Edge>, grid: Grid, edgesFrom: EdgeGrid): Int {
  while (!heap.isEmpty()) {
    val edgeIn = checkNotNull(heap.pollFirst())
    if (edgeIn.dirCount >= MIN_STRAIGHT && edgeIn.dest == Point(grid.size-1, grid[0].size-1)) {
      return edgeIn.distance
    }
    check(edgeIn.distance != Int.MAX_VALUE) {
      "Edge is unexpetedly max value: $edgeIn."
    }
    for (edgeOut in availableEdgesFrom(edgeIn, edgesFrom)) {
      val newDistance = edgeIn.distance + grid[edgeOut.dest.row][edgeOut.dest.col]
      if (newDistance < edgeOut.distance) {
        check(heap.remove(edgeOut)) { "edge not in heap: $edgeOut (reached from $edgeIn)" }
        edgeOut.distance = newDistance
        heap.add(edgeOut)
      }
    }
  }
  error("never reached bottom right corner")
}


private fun distanceCompare(e1: Edge, e2: Edge): Int =
  compareValuesBy<Edge>(e1, e2, {it.distance}, {it.uniqueId})

fun main() {
  val grid: Grid = generateSequence(::readLine).map { line ->
    line.map { it.digitToInt() }
  }.toList()
  val edgesFrom = allEdgesFrom(grid)

  val heap = TreeSet<Edge>(::distanceCompare)
  heap.addAll(edgesFrom.asSequence().flatten().flatten())
  println(runDijkstra(heap, grid, edgesFrom))
}
