// reusing binSearch from part 1

fun main() {
  val time = readLine()!!.removePrefix("Time:").replace(" ", "").toLong()
  val distance = readLine()!!.removePrefix("Distance:").replace(" ", "").toLong()

  println(binSearch(time, distance))
}
