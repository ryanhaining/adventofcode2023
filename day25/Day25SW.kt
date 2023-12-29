// Stoer-Wagner

private data class Edge(var weight: Int = 1)

private data class SearchResult(
    val s: String,
    val t: String,
    val weight: Int,
)

private fun maxAdjacencySearch(edgesByNode: Map<String, Map<String, Edge>>): SearchResult {
  val candidates = edgesByNode.keys.toMutableSet()
  val start = candidates.take(1).single()
  candidates.remove(start)
  val used = mutableListOf(start)
  var cutWeight: Int? = null

  val adjWeights: MutableMap<String, Int> =
      edgesByNode[start]!!.mapValues { (_, e) -> e.weight }.toMutableMap()

  while (!candidates.isEmpty()) {
    val (maxNode, maxWeight) = adjWeights.maxByOrNull { (_, w) -> w }!!
    candidates.remove(maxNode)
    adjWeights.remove(maxNode)
    used.add(maxNode)
    cutWeight = maxWeight
    // We don't need to calculate new weights for every candidate every time.
    // Only the weights for the newly used node's neighbors will change
    for ((dest, e) in edgesByNode[maxNode]!!) {
      if (dest in candidates) {
        var w = adjWeights.getOrPut(dest) { 0 }
        w += e.weight
        adjWeights[dest] = w
      }
    }
  }

  return SearchResult(used[used.size - 2], used.last(), cutWeight!!)
}

private fun findMinCutNodes(originalEdgesByNode: Map<String, Map<String, Edge>>): Int {
  val edgesByNode: MutableMap<String, MutableMap<String, Edge>> =
      originalEdgesByNode.mapValues { it.value.toMutableMap() }.toMutableMap()

  var cutNodes = 0
  var bestCutNodes = 0
  var bestCut: SearchResult? = null
  while (edgesByNode.size > 1 && bestCut?.weight != 3) {
    val sr = maxAdjacencySearch(edgesByNode)
    ++cutNodes
    if (bestCut == null || sr.weight < bestCut.weight) {
      bestCut = sr
      bestCutNodes = cutNodes
    }
    mergeNodes(edgesByNode, sr.s, sr.t)
  }

  check(bestCut?.weight == 3) { "Expected best cut with weight 3 but got $bestCut" }

  return bestCutNodes
}

private fun mergeNodes(
    edgesByNode: MutableMap<String, MutableMap<String, Edge>>,
    s: String,
    t: String
) {
  require(s != t) { "got same node twice: $s" }
  val tEdges = edgesByNode[t]!!.toMap()
  val sEdges = edgesByNode[s]!!
  for ((dest, tEdge) in tEdges) {
    val sEdge = sEdges[dest]
    if (sEdge != null) {
      // s is also connected, combine weights
      sEdge.weight += tEdge.weight
    } else {
      // s is not connected. connect it.
      sEdges[dest] = tEdge
      edgesByNode[dest]!![s] = tEdge
    }
  }

  for (m in edgesByNode.values) {
    m.remove(t)
  }
  edgesByNode.remove(t)
}

fun main() {
  val edges = mutableMapOf<Set<String>, Edge>() // Node,Node -> Weight
  generateSequence(::readLine).forEach { line ->
    val (k, restStr) = line.split(": ")
    val rest = restStr.split(" ")
    for (n in rest) {
      edges[setOf(k, n)] = Edge()
    }
  }

  val edgesByNode = mutableMapOf<String, MutableMap<String, Edge>>()
  for ((ns, e) in edges) {
    val (n1, n2) = ns.toList()
    edgesByNode.getOrPut(n1) { mutableMapOf() }[n2] = e
    edgesByNode.getOrPut(n2) { mutableMapOf() }[n1] = e
  }

  // The -3 is because the cut nodes includes both sides of the cut connection.
  // Not sure if that makes sense for this algorithm or not.
  val cutNodes = findMinCutNodes(edgesByNode) - 3

  println(cutNodes * (edgesByNode.size - cutNodes))
}
