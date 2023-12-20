private enum class Pulse { HIGH, LOW }

private data class Signal(val src: Module, val dest: Module, val pulse: Pulse)

private sealed class Module(val label: String, val destLabels: List<String>) {
  lateinit var dests: List<Module>
  open fun setupRefs(modules: Map<String, Module>) {
    dests = destLabels.map { modules[it] ?: SINK }
  }

  abstract fun process(signal: Signal): List<Signal>
}

private class Normal(label: String, destLabels: List<String>)
  : Module(label, destLabels) {
    override fun process(signal: Signal): List<Signal> =
      dests.map { d -> Signal(src=this, dest=d, signal.pulse) }

  override fun toString() = "Normal($label)"
}

private object SINK : Module("SINK", listOf()) {
  init {
    dests = listOf()
  }
  override fun process(signal: Signal) = listOf<Signal>()

  override fun toString() = "SINK"
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
  private fun setupReceived(modules: Map<String, Module>) {
    received = modules.values.filter { label in it.destLabels }.map { it.label }.associateWith { Pulse.LOW }.toMutableMap()
  }

  override fun setupRefs(modules: Map<String, Module>) {
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

fun main() {
  val modules = generateSequence(::readLine).map { line ->
    val (label, destsStr) = line.split(" -> ")
    val destLabels = destsStr.split(", ")
    when (label[0]) {
      '%' -> FlipFlop(label.substring(1), destLabels)
      '&' -> Conjunction(label.substring(1), destLabels)
      else -> Normal(label, destLabels)
    }
  }.associateBy {it.label}
  for (m in modules.values) {
    m.setupRefs(modules)
  }
  val startButton = StartButton(modules["broadcaster"]!!)
  var highs = 0
  var lows = 0
  repeat(1000) {
    val signals = ArrayDeque<Signal>(startButton.press())
    while (signals.isNotEmpty()) {
      val s = signals.removeFirst()
      when (s.pulse) {
        Pulse.HIGH -> ++highs
        Pulse.LOW -> ++lows
      }
      signals.addAll(s.dest.process(s))
    }
  }
  println(highs * lows)
}
