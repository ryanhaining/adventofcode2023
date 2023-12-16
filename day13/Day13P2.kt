private data class Pivot(val leftMid: Int, var smudgeUsed: Boolean=false) {
  val rightMid: Int = leftMid+1
}

private fun pivotIsValid(line: String, pivot: Pivot): Boolean {
  if (line[pivot.leftMid] != line[pivot.rightMid]) {
    if (pivot.smudgeUsed) {
      return false
    }
    pivot.smudgeUsed = true
  }
  var size = 0
  while (pivot.leftMid-size != 0 && pivot.rightMid+size != line.length-1) {
   if (line[pivot.leftMid-(size+1)] != line[pivot.rightMid+(size+1)]) {
     if (pivot.smudgeUsed) {
       return false
     }
     pivot.smudgeUsed = true
   }
   ++size;
  }
  if (pivot.leftMid-size != 0 && pivot.rightMid+size != line.length-1) {
    return false
  }
  return true
}

private fun findPivots(line: String): List<Pivot> =
  (0 ..< line.length-1).map { Pivot(it) }.filter {
    // note: not referentially transparent, may change it.smudgeUsed
    pivotIsValid(line, it)
  }

private fun findPivotEdge(area: List<String>): Int? {
    var pivots = findPivots(area[0])
    var first = true
    for (line in area) {
      if (first) {
        first = false
        continue
      }
      pivots = pivots.filter { pivotIsValid(line, it) }
    }
    pivots = pivots.filter { it.smudgeUsed }
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
