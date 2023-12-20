private enum class Pulse { HIGH, LOW }

private data class Signal(val src: Module, val dest: Module, val pulse: Pulse)

private sealed class Module(val label: String, val destLabels: List<String>) {
  lateinit var dests: List<Module>
  open fun setupRefs(modules: MutableMap<String, Module>) {
    dests = destLabels.map { modules.getOrPut(it) { Sink(it) } }
  }

  abstract fun process(signal: Signal): List<Signal>
}

open private class Normal(label: String, destLabels: List<String>)
  : Module(label, destLabels) {
    override fun process(signal: Signal): List<Signal> =
      dests.map { d -> Signal(src=this, dest=d, signal.pulse) }

  override fun toString() = "Normal($label)"
}

private class Sink(label: String) : Normal(label, listOf()) {
  init {
    dests = listOf()
  }
}

private class StartButton(broadCaster: Module)
  : Module("button", listOf("broadcaster")) {
  init {
    dests = listOf(broadCaster)
  }
  fun press() = listOf(Signal(this, dests.single(), Pulse.LOW))

  override fun process(signal: Signal): Nothing = error("start button cannot process")
}

private class FlipFlop(label: String, destLabels: List<String>)
  : Module(label, destLabels) {
  var state = false

  override fun process(signal: Signal): List<Signal> =
    when (signal.pulse) {
      Pulse.HIGH -> listOf()
      Pulse.LOW -> {
        state = !state
        dests.map { d ->
          Signal(src=this, dest=d, if (state) Pulse.HIGH else Pulse.LOW)
        }
      }
    }
  override fun toString() = "FlipFlop($label, $state)"
}


private class Conjunction(label: String, destLabels: List<String>)
  : Module(label, destLabels) {
  lateinit var received: MutableMap<String, Pulse>
  private fun setupReceived(modules: MutableMap<String, Module>) {
    received = modules.values.filter { label in it.destLabels }.map { it.label }.associateWith { Pulse.LOW }.toMutableMap()
  }

  override fun setupRefs(modules: MutableMap<String, Module>) {
    super.setupRefs(modules)
    setupReceived(modules)
  }

  override fun process(signal: Signal): List<Signal> {
    received[signal.src.label] = signal.pulse
    val nextPulse = if (received.values.all { it == Pulse.HIGH }) Pulse.LOW else Pulse.HIGH
    return dests.map { d -> Signal(src=this, dest=d, nextPulse) }
  }

  override fun toString() = "Conjunction($label, $received)"
}

// prior art
private fun gcd(x: Long, y: Long): Long = if (y == 0L) x else gcd(y, x % y)
private fun lcm(x: Long, y: Long): Long = x * (y / (gcd(x, y)))

fun main() {
  val modules = generateSequence(::readLine).map { line ->
    val (label, destsStr) = line.split(" -> ")
    val destLabels = destsStr.split(", ")
    when (label[0]) {
      '%' -> FlipFlop(label.substring(1), destLabels)
      '&' -> Conjunction(label.substring(1), destLabels)
      else -> Normal(label, destLabels)
    }
  }.associateBy {it.label}.toMutableMap()
  for (m in modules.values.toList()) {
    m.setupRefs(modules)
  }
  val startButton = StartButton(modules["broadcaster"]!!)

  // We could probably make this more efficient by going up further and
  // further, but once we get to the 4 conjuctions that feed the one conjunction
  // that feeds rx, we find their "HIGH" cycle point on the order of thousands
  // of iterations, and that's fast enough.
  val goal = modules.values.first{ it.label == "rx" }
  val goalParent = modules.values.filter { goal in it.dests }.single()
  val numGoalGrandparents = (goalParent as Conjunction).received.keys.size

  val produceHigh = mutableMapOf<String, Long>()
  var presses = 1L
  while (produceHigh.size < numGoalGrandparents) {
    val signals = ArrayDeque<Signal>(startButton.press())
    while (signals.isNotEmpty()) {
      val s = signals.removeFirst()
      signals.addAll(s.dest.process(s))
      if (s.dest === goalParent && s.pulse == Pulse.HIGH) {
        produceHigh[s.src.label] = presses
      }
    }
    ++presses
  }
  println(produceHigh.values.reduce(::lcm))
}
