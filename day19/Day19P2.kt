private val WORKFLOW_PATTERN = Regex("""(\w+)\{(.+)\}""")

private typealias Gear = List<Int>

private const val GEAR_LABELS = "xmas"

private fun toGearIndex(c: Char): Int {
  val index = GEAR_LABELS.indexOf(c)
  check(index >= 0) { "Unsupported gear label: $c" }
  return index
}

private enum class Comparison {
  LT,
  GT;
  companion object {
    fun from(c: Char): Comparison = when(c) {
      '>' -> GT
      '<' -> LT
      else -> error("Usupported comparison char: $c")
    }
  }
}

private data class Condition(val index: Int, val comparison: Comparison, val value: Int, val destLabel: String) {
  lateinit var dest: State
  private val clamp: IntRange = when (comparison) {
      Comparison.LT -> 1 ..< value
      Comparison.GT -> value+1 .. 4000
    }

  fun shrinkToFit(limits: List<IntRange>): List<IntRange>? {
    val result = limits.toMutableList()
    val current = result[index]
    result[index] = maxOf(clamp.start, current.start)..minOf(clamp.endInclusive, current.endInclusive)
    if (result[index].isEmpty()) {
      // NOTE: this never occurs for the actual input
      return null
    }
    return result
  }

  fun shrinkToNotFit(limits: List<IntRange>): List<IntRange>? {
    val result = limits.toMutableList()
    val current = result[index]
    result[index] = when (comparison) {
      Comparison.GT -> current.start..minOf(clamp.start-1, current.endInclusive)
      Comparison.LT -> maxOf(clamp.endInclusive+1, current.start)..current.endInclusive

    }
    if (result[index].isEmpty()) {
      // NOTE: this never occurs for the actual input
      return null
    }
    return result
  }

  companion object {
    fun from(s: String): Condition {
      val gearIndex=toGearIndex(s[0])
      val comparison=Comparison.from(s[1])
      val (valueStr, destLabel) = s.substring(2).split(":")
      return Condition(gearIndex, comparison, valueStr.toInt(), destLabel)
    }
  }
}

private sealed interface State {
  fun findAccepts(limits: List<IntRange>): List<List<IntRange>>
}
private object ACCEPT: State {
  override fun findAccepts(limits: List<IntRange>): List<List<IntRange>> = listOf(limits)
}
private object REJECT: State {
  override fun findAccepts(limits: List<IntRange>): List<List<IntRange>> = listOf()
}

private class Workflow(val label: String, val conditions: List<Condition>, val defaultStateLabel: String) : State {
  lateinit var defaultState: State
  fun setPointers(workflows: Map<String, State>) {
    defaultState = checkNotNull(workflows[defaultStateLabel]) { "$defaultStateLabel not found" }
    for (c in conditions) {
      c.dest = checkNotNull(workflows[c.destLabel]) { "${c.destLabel} not found" }
    }
  }

  override fun findAccepts(limits: List<IntRange>): List<List<IntRange>> {
    var current = limits
    val result = mutableListOf<List<IntRange>>()
    for (c in conditions) {
      val inside = c.shrinkToFit(current)
      if (inside != null) {
        result += c.dest.findAccepts(inside)
      }
      val outside = c.shrinkToNotFit(current)
      if (outside == null) {
        return result
      }
      current = outside
    }
    result += defaultState.findAccepts(current)
    return result
  }

  companion object {
    fun from(line: String): Workflow {
      val match = WORKFLOW_PATTERN.matchEntire(line)!!.groupValues
      val name = match[1]
      val operations = match[2].split(",")
      val conditions = operations.slice(0..<operations.size-1).map { Condition.from(it) }
      val defaultStateLabel = operations.last()
      return Workflow(name, conditions, defaultStateLabel)
    }
  }
}

private fun readWorkflows(): List<Workflow> {
  val workflows = generateSequence {
    val line = readLine()
    if (line?.isEmpty() ?: true) null else line
  }.map {
    Workflow.from(it)
  }.toList()
  val m = mapOf<String, State>("A" to ACCEPT, "R" to REJECT) + workflows.associateBy{it.label}
  for (w in workflows) {
    w.setPointers(m)
  }
  return workflows
}

fun main() {
  val workflows = readWorkflows()
  val initialState = workflows.first { it.label == "in" }
  val result = initialState.findAccepts(List(4) { 1..4000 }).map { limits ->
    limits.fold(1L) { acc, rng -> acc * (rng.endInclusive+1 - rng.start) }
  }.sum()
  println(result)

}
