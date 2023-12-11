// the actual x-to-y mapping doesn't matter because they come in order
// we just can't modify a single value twice within one category
// so we reset `touched` every time we hit a blankline
private data class Value(var number: Long, var touched: Boolean = false)

fun main() {
  val current: List<Value> = readLine()!!.removePrefix("seeds: ").split(" ").map { Value(it.toLong()) }
  for (line in generateSequence(::readLine)) {
    if (line.isEmpty()) {
      readLine() // skip x-to-y map: line
      for (v in current) {
        v.touched = false
      }
    } else {
      val parts = line.split(" ").map{ it.toLong() }
      assert(parts.size == 3)
      val srcStart = parts[1]
      val destStart = parts[0]
      val rangeLength = parts[2]

      val offset = destStart - srcStart
      val range = srcStart .. (srcStart + rangeLength)

      for (v in current) {
        if (!v.touched && v.number in range) {
          v.number += offset
          v.touched = true
        }
      }
    }
  }
  println(current.minByOrNull { it.number }!!.number)
}
