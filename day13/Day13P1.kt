private data class Pivot(val leftMid: Int) {
  val rightMid: Int = leftMid+1
}

private fun pivotIsValid(line: String, pivot: Pivot): Boolean {
  if (line[pivot.leftMid] != line[pivot.rightMid]) {
    return false
  }
  var size = 0
  while (pivot.leftMid-size != 0 && pivot.rightMid+size != line.length-1
   && line[pivot.leftMid-(size+1)] == line[pivot.rightMid+(size+1)]) {
     ++size;
  }
  if (pivot.leftMid-size != 0 && pivot.rightMid+size != line.length-1) {
    return false
  }
  return true
}

private fun findPivots(line: String): List<Pivot> =
  (0..<line.length-1).map { Pivot(it) }.filter{ pivotIsValid(line, it) }

private fun findPivotEdge(area: List<String>): Int? {
    var pivots = findPivots(area[0])
    for (line in area) {
      pivots = pivots.filter { pivotIsValid(line, it) }
    }
    assert(pivots.size < 2)
    return pivots.singleOrNull()?.leftMid?.plus(1)
}

private fun readArea(): List<String>? {
  val grid = generateSequence {
    val line = readLine()
    if (line?.isEmpty() ?: true) null else line
  }.toList()
  return if (grid.isEmpty()) null else grid
}

private fun rotate(area: List<String>): List<String> =
  (0 ..< area[0].length).map { col ->
    (0 ..< area.size).map { row ->
      area[row][col]
    }.joinToString("")
  }

fun main() {
  val result = generateSequence(::readArea).map { area ->
    findPivotEdge(area) ?: 100*checkNotNull(findPivotEdge(rotate(area))) {
      "no reflection in\n${area.joinToString("\n")}\nor rotation\n${rotate(area).joinToString("\n")}"
    }
  }.sum()
  println(result)
}
