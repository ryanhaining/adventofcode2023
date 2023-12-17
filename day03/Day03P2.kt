sealed interface Tile
object Blank : Tile
object MaybeGear : Tile
class Num(var value: Int): Tile {
  fun addDigit(digit: Int) {
    value = value*10 + digit
  }
}

fun parseLine(line: String): List<Tile> =
  buildList {
    for (c in line) {
      if (c.isDigit()) {
        val end = lastOrNull() as? Num ?: Num(0)
        end.addDigit(c.digitToInt())
        add(end)
      } else if (c == '*') {
        add(MaybeGear)
      } else {
        add(Blank)
      }
    }
  }

fun sumGears(prev: List<Tile>, current: List<Tile>, next: List<Tile>): Int =
  current.withIndex().filter { (_, tile) -> tile is MaybeGear }.map {(i, _) ->
    val nums = (maxOf(0, i-1)..minOf(current.size, i+1)).map { col ->
      listOf(prev, current, next).mapNotNull { it[col] as? Num }
    }.flatten().toSet().toList()
    if (nums.size == 2) nums[0].value * nums[1].value else 0
  }.sum()

fun main() {
  var current: List<Tile> = parseLine(readLine()!!)
  var prev: List<Tile> = List(current.size) { Blank }
  val total = (generateSequence(::readLine)+sequenceOf(".".repeat(current.size))).map { line ->
    val next: List<Tile> = parseLine(line)
    val result = sumGears(prev, current, next)
    prev = current
    current = next
    result
  }.sum()
  println(total)
}
