sealed interface Tile
object Blank : Tile
object Symbol : Tile
data class Num(var value: Int): Tile {
  fun addDigit(digit: Int) {
    value = value*10 + digit
  }

  fun consume(): Int {
    val v = value
    value = 0
    return v
  }
}

fun parseLine(line: String): List<Tile> =
  buildList {
    for (c in line) {
      if (c.isDigit()) {
        val end = lastOrNull() as? Num ?: Num(0)
        end.addDigit(c.digitToInt())
        add(end)
      } else if (c == '.') {
        add(Blank)
      } else {
        add(Symbol)
      }
    }
  }

fun sumParts(prev: List<Tile>, current: List<Tile>, next: List<Tile>): Int =
  current.withIndex().filter { (_, tile) -> tile is Symbol }.map {(i, _) ->
    (maxOf(0, i-1)..minOf(current.size, i+1)).map { col ->
      listOf(prev, current, next).mapNotNull { (it[col] as? Num)?.consume() }.sum()
    }.sum()
  }.sum()

fun main() {
  var current: List<Tile> = parseLine(readLine()!!)
  var prev: List<Tile> = List(current.size) { Blank }
  val total = (generateSequence(::readLine)+sequenceOf(".".repeat(current.size))).map { line ->
    val next: List<Tile> = parseLine(line)
    val result = sumParts(prev, current, next)
    prev = current
    current = next
    result
  }.sum()
  println(total)
}
