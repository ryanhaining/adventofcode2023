// score is not the "rank" but just a way to order by type of hand
// it's the sum of the squares of the runs which favors bigger groups of common cards
// five of a kind is 5*5 = 25
// four of a kind is 4*4 + 1 = 17
// full house is 3*3 + 2*2 = 13
// three of a kind is 3*3 +1 + 1 = 11
// two pair is 2*2 + 2*2 + 1 + 1= 10
// one pair is 2*2 +1+1+1 = 7
// nothing is 1+1+1+1 = 5
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
      'J' -> 0
      'T' -> 10
      'Q' -> 12
      'K' -> 13
      'A' -> 14
      else -> error("Unsupported card: $c")
  }
}

private fun calcScore(cards: List<Int>): Int {
  val grouped = cards.groupBy { it }
  val numJokers = grouped[0]?.size ?: 0
  // filter out all the jokers
  val sizes = grouped.filter { (k, _) -> k != 0 }.map { (_, v) -> v.size }.toMutableList()
  // add jokers to whatever the largest group of cards is
  if (sizes.isNotEmpty()) {
    val maxIndex = sizes.withIndex().maxByOrNull { it.value }!!.index
    sizes[maxIndex] += numJokers
  } else {
    // special case: JJJJJ
    sizes.add(5)
  }
  return sizes.map {it * it}.sum()

}

fun main() {
  val result = generateSequence(::readLine).map {
    val (cardsStr, bidStr) = it.split(" ")
    val cards = toCards(cardsStr)
    val score = calcScore(cards)
    Hand(cards, score, bidStr.toLong())
  }.toList().sorted().withIndex().map {(it.index+1) * it.value.bid}.reduce {acc, n -> acc + n }
  println(result)
}
