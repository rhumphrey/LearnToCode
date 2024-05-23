@main def loops(): Unit = {
    // Iterating over a collection with a single line
    for i <- 1 to 5 do println(i)

    // Using a multiline for loop with a condition
    for
        i <- 1 to 10
        if i % 2 == 0
    do
        println(s"Even number: $i")
    end for // 'end for' can be used to improve readability
  }