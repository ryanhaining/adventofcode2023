private data class Load(var numRocks: Int, var offset: Int) {
  val hasRocks get(): Boolean = numRocks != 0
  fun weight(topWeight: Int): Int {
    val end = topWeight-offset
    val start = topWeight-offset-numRocks
    return ((end-start)*(start+end+1))/2
  }
}

fun main() {
  lateinit var loads: List<MutableList<Load>>
  var first = true
  val length = generateSequence(::readLine).withIndex().map { (row, line) ->
    if (first) {
      loads = List(line.length) { mutableListOf(Load(0, 0)) }
      first = false
    }
    for ((i, c) in line.withIndex()) {
      if (c == 'O') {
        ++loads[i].last().numRocks
      } else if ( c == '#') {
        if (loads[i].last().hasRocks) {
          loads[i] += Load(numRocks=0, offset=row+1)
        } else {
          loads[i].last().offset = row+1
        }
      }
    }
    1
  }.sum()

  val result = loads.map { lst ->
    lst.map { load ->
      load.weight(length)
    }.sum()
  }.sum()
  println(result)
}
