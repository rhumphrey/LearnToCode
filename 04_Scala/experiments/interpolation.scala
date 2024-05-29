// Based on examples from https://docs.scala-lang.org/scala3/book/string-interpolation.html
@main def interpolation() =
  // Scala provides three string interpolation methods out of the box: s, f and raw. 
  // A string interpolator is a just special method so it is possible to define your own. 
  // Prepending s to any string literal allows the usage of variables directly in the string.
  val name = "Socket"
  val what = "Corgi"
  val age  = 5
  println(s"$name the $what is $age years old")  

  // String interpolators can also take arbitrary expressions.
  // Any arbitrary expression can be embedded in ${}
  println(s"2 + 2 = ${2 + 2}")   // "2 + 2 = 4"
  val x = -1
  println(s"x     = $x")
  println(s"x.abs = ${x.abs}")   // "x.abs = 1"

  // In some instances it may be necessary to 'escape' certain characters you might need
  println(s"New offers starting at $$14.99")   // we need to use $$ because we want a dollar ($) sign
  // Double quotes also need to be escaped. This can be done by using triple quotes as shown
  println(s"""{"name":"Socket"}""")        

  // Multiline string use is also possible with interpolators (this example also uses """)
  println(s"""name: "$name",
             |what: "$what",
             |age : $age""".stripMargin)

  // Prepending f to any string literal allows the creation of simple formatted strings, 
  // similar to printf in other languages
  // Note: you can use %% to get a literal % character in the output string, if needed.
  val height = 1.8d
  val aName = "Ted"
  println(f"$aName%s is $height%2.2f metres tall") 
  println(f"3/19 is less than 20%%")

  // The raw interpolator is similar to the s interpolator except that it performs 
  // no escaping of literals within the string.
  // useful when you want to avoid having expressions like \n turn into a return character.
    val string = s"a\nb"
  val rawstring = raw"a\nb"
  println(string)
  println(rawstring)

  // Custom Interpolator example
  // Define a case class `Point` with two parameters `x` and `y` of type Double.
  case class Point(x: Double, y: Double)

  // Define an extension method for the `StringContext` class.
  extension (sc: StringContext)
    // Define a method `p` that takes a variable number of `Double` arguments.
    def p(args: Double*): Point = 
      // Use the `s` string interpolator to substitute the values of `args` into the string,
      // then split the resulting string at the comma, limiting the result to 2 parts.
      // The `map` operation attempts to convert each string part to a `Double` using `toDoubleOption`,
      // defaulting to `0.0` if the conversion is not possible.
      val pts = sc.s(args*).split(",", 2).map { _.toDoubleOption.getOrElse(0.0) }
      // Create a new `Point` instance using the first two elements of the `pts` array.
      Point(pts(0), pts(1))
    

  val pointX = 12.0

  val pointString1 = p"1, -2"                  // Point(1.0, -2.0)
  val pointString2 = p"${pointX/5}, $pointX"   // Point(2.4, 12.0)
  println(pointString1)
  println(pointString2)