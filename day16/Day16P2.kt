private enum class Direction {
  down, up, left, right
}

private data class Tile(val c: Char, val energized: MutableSet<Direction> = mutableSetOf())

private fun nextDirections(c: Char, d: Direction): List<Direction> =
  when (c) {
    '.' -> listOf(d)
    '/' -> when(d) {
      Direction.up -> listOf(Direction.right)
      Direction.left -> listOf(Direction.down)
      Direction.right -> listOf(Direction.up)
      Direction.down -> listOf(Direction.left)
    }
    '\\' -> when(d) {
      Direction.up -> listOf(Direction.left)
      Direction.left -> listOf(Direction.up)
      Direction.right -> listOf(Direction.down)
      Direction.down -> listOf(Direction.right)
    }
    '|' -> when(d) {
      Direction.up, Direction.down -> listOf(d)
      Direction.left, Direction.right -> listOf(Direction.up, Direction.down)
    }
    '-' -> when(d) {
      Direction.left, Direction.right -> listOf(d)
      Direction.up, Direction.down -> listOf(Direction.left, Direction.right)
    }
    else -> error("Unknown char: $c")
  }

private data class Point(val row: Int, val col: Int)

private fun nextPoint(p: Point, d: Direction) =
  when(d) {
    Direction.up -> Point(p.row-1, p.col)
    Direction.down -> Point(p.row+1, p.col)
    Direction.left -> Point(p.row, p.col-1)
    Direction.right -> Point(p.row, p.col+1)
  }

private fun traverse(tiles: List<List<Tile>>, p: Point, d: Direction) {
  if (p.row < 0 || p.col < 0 || p.row >= tiles.size || p.col >= tiles[p.row].size) {
    return
  }
  if (d in tiles[p.row][p.col].energized) {
    return
  }
  tiles[p.row][p.col].energized.add(d)
  val nextDirections = nextDirections(tiles[p.row][p.col].c, d)
  val nextPoints = nextDirections.map { nextDir -> nextPoint(p, nextDir) }
  for ((nextDir, nextPt) in nextDirections.zip(nextPoints)) {
    traverse(tiles, nextPt, nextDir)
  }
}

private fun countEnergized(tiles: List<List<Tile>>): Int =
  tiles.map { row ->
    row.count { it.energized.isNotEmpty() }
  }.sum()
    
private fun reset(tiles: List<List<Tile>>) {
  for (row in tiles) {
    for (t in row) {
      t.energized.clear()
    }
  }
}

fun main() {
  val tiles: List<List<Tile>> = generateSequence(::readLine).map { line->
    line.map { Tile(it) }
  }.toList()
  // brute force is fast enough for the input.
  // this runs in <1s for me.
  // I thought this was gonna be DP for sure and was trying to figure it out
  // while writing this, thinking "It seems like it would be fast enough but
  // that would be too easy"
  val sidesResult = (0 ..< tiles.size).map { row ->
    traverse(tiles, Point(row, 0), Direction.right)
    val leftEdgeResult = countEnergized(tiles)
    reset(tiles)
    traverse(tiles, Point(row, tiles[row].size), Direction.left)
    val rightEdgeResult = countEnergized(tiles)
    reset(tiles)
    maxOf(leftEdgeResult, rightEdgeResult)
  }.max()
  val topBottomResult = (0 ..< tiles[0].size).map { col ->
    traverse(tiles, Point(0, col), Direction.down)
    val topEdgeResult = countEnergized(tiles)
    reset(tiles)
    traverse(tiles, Point(tiles.size, col), Direction.up)
    val bottomEdgeResult = countEnergized(tiles)
    reset(tiles)
    maxOf(topEdgeResult, bottomEdgeResult)
  }.max()
  println(maxOf(topBottomResult, sidesResult))


}
