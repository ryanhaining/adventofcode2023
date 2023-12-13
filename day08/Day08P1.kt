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
  var steps = 0
  var currentNode = nodes["AAA"]!!
  while (currentNode.name != "ZZZ") {
    for (d in dirs) {
      if (currentNode.name == "ZZZ") {
        break;
      }
      currentNode = when (d) {
        Direction.LEFT -> currentNode.left!!
        Direction.RIGHT -> currentNode.right!!
      }
      ++steps
    }
  }
  println(steps)
}
