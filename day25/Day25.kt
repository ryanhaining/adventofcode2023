import java.util.TreeSet

private data class Node(val label: String) {
  val neighbors = mutableSetOf<Node>()
  var distance: Int = Int.MAX_VALUE
}

private fun MutableMap<String, Node>.getNode(label: String) = getOrPut(label) { Node(label) }

private fun findDistance(heap: TreeSet<Node>, dest: Node): Int {
  while (true) {
    val current = checkNotNull(heap.pollFirst())
    if (current === dest) {
      return current.distance
    }
    val newDistance = current.distance + 1
    for (n in current.neighbors) {
      if (newDistance < n.distance) {
        check(heap.remove(n))
        n.distance = newDistance
        check(heap.add(n))
      }
    }
  }
}

private fun findSize(current: Node): Int {
  if (current.distance == 0) {
    return 0
  }
  current.distance = 0
  return 1 + current.neighbors.map(::findSize).sum()
}

private fun distanceCompare(n1: Node, n2: Node): Int =
    compareValuesBy<Node>(n1, n2, { it.distance }, { it.label })

private fun resetDistances(nodes: Collection<Node>) {
  for (n in nodes) {
    n.distance = Int.MAX_VALUE
  }
}

fun main() {
  val m = mutableMapOf<String, Node>()
  generateSequence(::readLine).forEach { line ->
    val (k, restStr) = line.split(": ")
    val rest = restStr.split(" ")
    val src = m.getNode(k)
    for (n in rest) {
      val dest = m.getNode(n)
      src.neighbors.add(dest)
      dest.neighbors.add(src)
    }
  }

  val distances = mutableMapOf<Set<Node>, Int>()

  for (src in m.values) {
    for (dest in src.neighbors.toList()) {
      val pair = setOf(src, dest)
      if (distances.containsKey(pair)) {
        continue
      }
      src.neighbors.remove(dest)
      dest.neighbors.remove(src)

      src.distance = 0
      val heap = TreeSet<Node>(::distanceCompare).apply { addAll(m.values) }
      distances[pair] = findDistance(heap, dest)

      src.neighbors.add(dest)
      dest.neighbors.add(src)

      resetDistances(m.values)
    }
  }

  val toCut =
      distances
          .toList()
          .sortedBy { (_, v) -> -v }
          .take(3)
          .map { (k, _) -> k.toList() }

  for ((n1, n2) in toCut) {
    n1.neighbors.remove(n2)
    n2.neighbors.remove(n1)
  }
  val firstSize = findSize(toCut[0][0])
  val secondSize = findSize(toCut[0][1])
  check(firstSize + secondSize == m.values.size) {
    "sum of subgraph sizes != number of nodes. " +
        "Expected total: ${m.values.size} but got $firstSize and $secondSize"
  }
  println(firstSize * secondSize)
}
