// the actual x-to-y mapping doesn't matter because they come in order
// we just can't modify a single value twice within one category
// so we reset `touched` every time we hit a blankline
private data class Value(var range: LongRange, var touched: Boolean = false)

private fun mergedRanges(vs: List<Value>): List<Value> = buildList {
  var currentStart = vs[0].range.start
  for ((cur, next) in vs.zipWithNext()) {
    if (cur.range.endInclusive+1 < next.range.start) {
      add(Value(currentStart .. cur.range.endInclusive))
      currentStart = next.range.start
    }
  }
  if (isEmpty() || last().range.endInclusive != vs.last().range.endInclusive) {
      add(Value(currentStart .. vs.last().range.endInclusive))
  }
}

fun main() {
  var current: List<Value> = readLine()!!
  .removePrefix("seeds: ")
  .split(" ")
  .map { it.toLong() }
  .windowed(2, 2) { Value(it[0] .. it[0]+it[1]) }
  .sortedBy { it.range.start }

  for (line in generateSequence(::readLine)) {
    if (line.isEmpty()) {
      readLine() // skip x-to-y map: line
      current = mergedRanges(current.sortedBy { it.range.start })
    } else {
      val parts = line.split(" ").map{ it.toLong() }
      assert(parts.size == 3)
      val srcStart = parts[1]
      val destStart = parts[0]
      val rangeLength = parts[2]

      val offset = destStart - srcStart
      val destRange = srcStart .. (srcStart + rangeLength)

      // If we only map part of a range, we may need to map the other part later
      // `remaining` accumulates the new subranges that we add while we break
      // bigger ranges apart.
      val remaining = ArrayDeque<Value>(current)
      current = buildList {
        while (!remaining.isEmpty()) {
          val v = remaining.removeFirst()
          if (v.touched) {
            add(v) 
          } else {
            if (v.range.start in destRange && v.range.endInclusive in destRange) {
              // the destination range fully covers the source range, just apply the offset
              add(Value(v.range.start+offset .. v.range.endInclusive+offset, true))
            }
            else if (destRange.start in v.range && destRange.endInclusive in v.range) {
              // the destination range fits inside the source range, we need three parts
              // [v.start, dest.start),
              // [dest.start+offset, dest.endInclusive+offset],
              // (dest.endInclusive, v.end]
              add(Value(destRange.start + offset .. destRange.endInclusive+offset, true))
              remaining.addLast(Value(v.range.start ..< destRange.start))
              remaining.addLast(Value(destRange.endInclusive+1 .. v.range.endInclusive))
            }
            else if (destRange.start in v.range) {
              // the destination starts in the middle of source range and ends after it
              add(Value(destRange.start+offset .. v.range.endInclusive + offset, true))
              remaining.addLast(Value(v.range.start ..< destRange.start))
            } else if (destRange.endInclusive in v.range) {
              // the destination starts before the source range and ends in the middle of it
              add(Value(v.range.start+offset .. destRange.endInclusive + offset, true))
              remaining.addLast(Value(destRange.endInclusive+1 .. v.range.endInclusive))
            } else {
              // destRange doesn't overlap with source range at all
              add(Value(v.range, false))
            }
          }
        }
      }
    }
  }
  println(current.minByOrNull { it.range.start }!!.range.start)
}
