import kotlin.math.absoluteValue

private data class Point(val row: Int, val col: Int): Comparable<Point> {
  override operator fun compareTo(other: Point): Int =
    compareValuesBy(this, other, {it.row}, {it.col})
}

private fun findOffsets(lst: List<Int>): Map<Int, Int> =
  lst.toSet().toList().sorted().withIndex().map { (i, num) -> num to num-i }.toMap()

fun main() {
  var points = generateSequence(::readLine).withIndex().flatMap {(row, line) ->
    line
    .withIndex()
    .filter { (_, chr) -> chr == '#' }
    .map { (col, _) -> Point(row, col) }
  }.toList()

  val rowOffsets = findOffsets(points.map{it.row})
  val colOffsets = findOffsets(points.map{it.col})

  points = points.map { it.copy(it.row + rowOffsets[it.row]!!, it.col + colOffsets[it.col]!!) }

  val total = points.flatMap { p1 ->
    points.map{ p2 ->
      if (p1 < p2) {
        (p2.row - p1.row).absoluteValue + (p2.col - p1.col).absoluteValue
      } else {
        0 
      }
    }
  }.sum()
  println(total)
}
