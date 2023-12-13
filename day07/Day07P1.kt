// score is not the "rank" but just a way to order by type of hand
private data class Hand(val cards: List<Int>, val score: Int, val bid: Long) : Comparable<Hand> {
  override operator fun compareTo(other: Hand): Int = compareValuesBy(this, other,
  {it.score },
  {it.cards[0]},
  {it.cards[1]},
  {it.cards[2]},
  {it.cards[3]},
  {it.cards[4]},
  )
}

private fun toCards(s: String) =
  s.map { c ->
    when (c) {
      in '2'..'9' -> c - '0'
      'T' -> 10
      'J' -> 11
      'Q' -> 12
      'K' -> 13
      'A' -> 14
      else -> error("Unsupported card: $c")
  }
}

fun main() {
  val result = generateSequence(::readLine).map {
    val (cardsStr, bidStr) = it.split(" ")
    val cards = toCards(cardsStr)
    val score = cards.groupBy { it }.values.map{it.size*it.size}.sum()
    Hand(cards, score, bidStr.toLong())
  }.toList().sorted().withIndex().map {(it.index+1) * it.value.bid}.reduce {acc, n -> acc + n }
  println(result)
}

