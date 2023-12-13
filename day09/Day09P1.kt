fun main() {
  println(generateSequence(::readLine).map { line ->
    val nums = line.split(" ").map { it.toLong() }.toMutableList()
    for (computeLength in nums.size-1 downTo 0) {
      for (i in 0 ..< computeLength) {
        nums[i] = nums[i+1] - nums[i]
      }
    }
    nums.sum()
  }.sum())
}
