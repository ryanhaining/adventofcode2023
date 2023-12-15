private data class CallKey(
    val runLength: Int,
    val springIndex: Int,
    val groupIndex: Int)

private val cache = mutableMapOf<CallKey, Long>()

private fun countPossibilities(runLength: Int, springs: String, springIndex: Int, groups: List<Int>, groupIndex: Int, clearCache: Boolean=false): Long {
  if (clearCache) {
    cache.clear()
  }
  val callKey = CallKey(runLength, springIndex, groupIndex)
  // now that's what I call Dynamic Programming
  // (not actually)
  val cached = cache[callKey]
  if (cached != null) {
    return cached
  }
  if (groupIndex == groups.size) {
    return (if ((springIndex..<springs.length).all { i -> springs[i] != '#' }) 1L else 0L).also {
      cache[callKey] = it }
  }
  if (springIndex == springs.length) {
    return 0L
  }

  val atLength = runLength == groups[groupIndex]
  if (springs[springIndex] == '#') {
    if (atLength) {
      return 0L
    }
    return countPossibilities(runLength+1, springs, springIndex+1, groups, groupIndex).also {
      cache[callKey] = it
    }
  }
  if (springs[springIndex] == '.') {
    if (runLength != 0 && !atLength)  {
      return 0L
    }
    return countPossibilities(0, springs, springIndex+1, groups, groupIndex + (if (runLength != 0) 1 else 0)).also {
      cache[callKey] = it
    }
  }
  check(springs[springIndex] == '?') {
    "unexpected character at index $springIndex in $springs: ${springs[springIndex]}"
  }
  if (atLength) {
     return countPossibilities(0, springs, springIndex+1, groups, groupIndex+1).also {
      cache[callKey] = it
    }
  }
  if (runLength != 0) {
    return countPossibilities(runLength+1, springs, springIndex+1, groups, groupIndex).also {
      cache[callKey] = it
    }
  } else {
     return (countPossibilities(0, springs, springIndex+1, groups,groupIndex) +
     countPossibilities(runLength+1, springs, springIndex+1, groups, groupIndex)).also {
      cache[callKey] = it
    }

  }
}
fun main() {
  val result = generateSequence(::readLine).map { line ->
    val (springsStr, groupsStr) = line.split(' ')
    val springs = List(5){springsStr}.joinToString("?") + '.'
    val singleGroups = groupsStr.split(',').map { it.toInt() }
    val groups = List(5) {singleGroups}.flatten()
    countPossibilities(runLength=0, springs, springIndex=0, groups, groupIndex=0, clearCache=true)
  }.sum()
  println(result)
}
