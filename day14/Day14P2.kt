private data class Load(var numRocks: Int, val topWeight: Int) {
  val hasRocks get(): Boolean = numRocks != 0
  val weight get(): Int {
    val end = topWeight
    val start = topWeight-numRocks
    return ((end-start)*(start+end+1))/2
  }
}

private fun readGrid(): List<List<Char>> =
  generateSequence(::readLine).map {
    it.toList()
  }.toList()

private fun renderRotated(loads: List<List<Load>>, length: Int): List<List<Char>> {
  val output = List(loads.size) { MutableList<Char>(length) { '.' } }
  loads.zip(output).forEach { (row, output) ->
    row.forEach { load ->
        if (load.topWeight != length) {
          output[length-load.topWeight-1] = '#'
        }
        for (i in (length-load.topWeight) ..< (length-load.topWeight+load.numRocks)) {
          output[i] = 'O'
        }
    }
  }
  return  output.map { it.reversed() }
}

private fun findLoads(grid: List<List<Char>>): List<List<Load>> {
  val length = grid[0].size
  val loads: List<MutableList<Load>> = List(length) { mutableListOf(Load(0, length)) }
  grid.withIndex().forEach { (row, line) ->
    for ((i, c) in line.withIndex()) {
      if (c == 'O') {
        ++loads[i].last().numRocks
      } else if ( c == '#') {
        loads[i] += Load(numRocks=0, topWeight=length-(row+1))
      }
    }
  }
  return loads
}

private fun countWeight(grid: List<List<Char>>): Int =
  grid.reversed().withIndex().map {(i, row) ->
    row.map { c ->
      if (c == 'O')  i+1 else 0
    }.sum()
  }.sum()

private fun runCycle(grid: List<List<Char>>): List<List<Char>> {
  var result = grid
  repeat(4) {
    val loads: List<List<Load>> = findLoads(result)
    result = renderRotated(loads, result[0].size)
  }
  return result
}

fun main() {
  // Instead of writing logic to rotate in all 4 directions, we do:
  // shift north, rotate, shift north, rotate ...
  var grid = readGrid()
  // I could make this faster with a map of Grid->Index,
  // but it indexOf is fast enough
  val uniqueGrids = mutableListOf<List<List<Char>>>(grid)
  var first: Int
  var last: Int
  while (true) {
    grid = runCycle(grid)
    val prevIndex = uniqueGrids.indexOf(grid)
    if (prevIndex >= 0) {
      first = prevIndex
      last = uniqueGrids.size
      break
    } else {
      uniqueGrids.add(grid)
    }
  }
  // Obviously we can't actually run this a billion times, so istead we find where
  // it stabilizes, and then calculate where it would be at cycle 1,000,000,000
  val distance = last-first
  val targetCycle = 1_000_000_000 - first
  val targetGrid = uniqueGrids[first + (targetCycle%distance) ]
  println(countWeight(targetGrid))
}
