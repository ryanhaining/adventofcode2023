private enum class Direction {
  LEFT,
  RIGHT;
  companion object {
    fun from(c: Char): Direction = when(c) {
      'L' -> LEFT
      'R' -> RIGHT
      else -> error("Unsupported character: $c")
    }
  }
}

private data class Node(val name: String, var left: Node?, var right: Node?)

// prior art
private fun gcd(x: Long, y: Long): Long = if (y == 0L) x else gcd(y, x % y)
private fun lcm(x: Long, y: Long): Long = x * (y / (gcd(x, y)))

fun main() {
  val nodes = mutableMapOf<String, Node>()
  val dirs = readLine()!!.map(Direction::from)
  readLine() // skip blank
  for (line in generateSequence(::readLine)) {
    val (current, left, right ) =
      Regex("""(\w\w\w) = \((\w\w\w), (\w\w\w)\)""").find(line)!!.destructured
    val currentNode = nodes.getOrPut(current) { Node(current, null, null) }
    currentNode.left = nodes.getOrPut(left) { Node(left, null, null) }
    currentNode.right = nodes.getOrPut(right) { Node(right, null, null) }
  }
  var steps = 0L
  var currentNodes = nodes.values.filter { it.name.endsWith('A') }
  val stepsToEnd = mutableListOf<Long>()
  while (currentNodes.isNotEmpty()) {
    for (d in dirs) {
      currentNodes = currentNodes.mapNotNull {
        if (it.name.endsWith('Z')) {
          stepsToEnd.add(steps)
          null
        } else {
          when (d) {
            Direction.LEFT -> it.left!!
            Direction.RIGHT -> it.right!!
          }
        }
      }
      ++steps
    }
  }
  println(stepsToEnd.reduce(::lcm))
}
