// There's gotta be an O(1) out there for finding the count
// but I only have CS degrees, not math, so all I can do is binary search
private fun isWin(chargeTime: Long, totalTime: Long, recordDistance: Long): Boolean {
  val timeLeft = totalTime - chargeTime
  val distance = chargeTime * timeLeft
  return distance > recordDistance
}

fun binSearch(totalTime: Long, recordDistance: Long): Long {
  var minCharge = 0L
  var maxCharge = totalTime/2 + 1
  while (true) {
    val mid = (maxCharge + minCharge)/2
    if (isWin(mid, totalTime, recordDistance)) {
      maxCharge = mid

      if (!isWin(mid-1, totalTime, recordDistance)) {
        return ((totalTime/2 + totalTime%2) - maxCharge) * 2 +
        if (totalTime%2 == 0L) 1 else 0
      }
    } else {
      minCharge = mid+1
    }
  }
}

fun main() {
  val times = readLine()!!.removePrefix("Time:").trimStart().split("\\s+".toRegex())
    .map { it.toLong() }
  val distances = readLine()!!.removePrefix("Distance:").trimStart().split("\\s+".toRegex())
    .map { it.toLong() }

  val result  = times.zip(distances).map {
    (time, dist)  -> binSearch(time, dist)
  }.reduce {
    acc, count -> acc * count
  }
  println(result)
}
