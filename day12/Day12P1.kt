private fun countPossibilities(runLength: Int, springs: String, springIndex: Int, groups: List<Int>, groupIndex: Int): Int {
  if (groupIndex == groups.size) {
    return if ((springIndex..<springs.length).all { i -> springs[i] != '#' }) 1 else 0
  }
  if (springIndex == springs.length) {
    // we're at the end of the springs but not at the end of the groups.
    return 0
  }

  val atLength = runLength == groups[groupIndex]
  if (springs[springIndex] == '#') {
    if (atLength) {
      // we're at the end of a run and there's another # after it.
      return 0
    }
    return countPossibilities(runLength+1, springs, springIndex+1, groups, groupIndex)
  }
  if (springs[springIndex] == '.') {
    if (runLength != 0 && !atLength)  {
      // we're on a . but we're in the middle of a run.
      return 0
    }
    return countPossibilities(0, springs, springIndex+1, groups, groupIndex + (if (runLength != 0) 1 else 0))
  }
  check(springs[springIndex] == '?') {
    "unexpected character at index $springIndex in $springs: ${springs[springIndex]}"
  }
  if (atLength) {
    // must treat as #
     return countPossibilities(0, springs, springIndex+1, groups, groupIndex+1)
  }
  if (runLength != 0) {
    // must treat as .
    return countPossibilities(runLength+1, springs, springIndex+1, groups, groupIndex)
  }
  // treat as either
  return countPossibilities(0, springs, springIndex+1, groups, groupIndex) +
  countPossibilities(runLength+1, springs, springIndex+1, groups, groupIndex)
}
fun main() {
  val result = generateSequence(::readLine).map { line ->
    val (springsStr, groupsStr) = line.split(' ')
    val springs = springsStr + '.' // extra . so we can check that we're done after the last #
    val groups = groupsStr.split(',').map { it.toInt() }
    countPossibilities(runLength=0, springs, springIndex=0, groups, groupIndex=0)
  }.sum()
  println(result)
}
