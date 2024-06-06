@main def sd() =
  // Define a method to calculate the mean of a list of numbers
  def mean(list: List[Double]): Double = list.sum / list.length

  // Define a method to calculate the variance of a list of numbers
  def variance(list: List[Double]): Double = {
    val meanValue = mean(list)
    list.map(num => math.pow(num - meanValue, 2)).sum / list.length
  }

  // Define a method to calculate the standard deviation of a list of numbers
  def standardDeviation(list: List[Double]): Double = math.sqrt(variance(list))

  // Define a method to calculate the median of a list of numbers
  def median(list: List[Double]): Double = {
    val sortedList = list.sorted
    val size = sortedList.length
    if (size % 2 == 0) (sortedList(size / 2 - 1) + sortedList(size / 2)) / 2
    else sortedList(size / 2)
  }

  // Test the code with an example list
  val list = List(1.0, 2.0, 3.0, 4.0, 5.0)
  println(f"The mean of $list is ${mean(list)}%.2f")
  println(f"The variance of $list is ${variance(list)}%.2f")
  println(f"The standard deviation of $list is ${standardDeviation(list)}%.2f")
  println(f"The median of $list is ${median(list)}%.2f")

