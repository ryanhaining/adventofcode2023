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
  fun eval(gear: Gear): Boolean =
    when (comparison) {
      Comparison.LT -> gear[index] < value
      Comparison.GT -> gear[index] > value
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

private sealed interface State
private object ACCEPT: State
private object REJECT: State

private class Workflow(val label: String, val conditions: List<Condition>, val defaultStateLabel: String) : State {
  lateinit var defaultState: State
  fun setPointers(workflows: Map<String, State>) {
    defaultState = checkNotNull(workflows[defaultStateLabel]) { "$defaultStateLabel not found" }
    for (c in conditions) {
      c.dest = checkNotNull(workflows[c.destLabel]) { "${c.destLabel} not found" }
    }
  }

  fun nextState(gear: Gear): State = conditions.firstOrNull { it.eval(gear) }?.dest ?: defaultState

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
  val result = generateSequence(::readLine).map { line ->
    val gear: Gear = line.slice(1..<line.length-1).split(",").map { it.substring(2).toInt() }
    var current: State = initialState
    while (current is Workflow) {
      current = current.nextState(gear)
    }
    when (current) {
      ACCEPT -> gear.sum()
      REJECT -> 0
      is Workflow -> error("should be impossible")
    }
  }.sum()
  println(result)

}

