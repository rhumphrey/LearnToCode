// Based on examples from https://docs.scala-lang.org/scala3/book/taste-modeling.html
@main def domain() =
  // OOP Domain Modeling 
  // Traits
  trait Speaker:
    def speak(): String  // has no body, so it’s abstract

  trait TailWagger:
    def startTail(): Unit = println("tail is wagging")
    def stopTail(): Unit = println("tail is stopped")

  trait Runner:
    def startRunning(): Unit = println("I'm running")
    def stopRunning(): Unit = println("Stopped running")

  // A Dog class that extends the previous traits + provides behaviour for speak()
  class Dog(val name: String) extends Speaker, TailWagger, Runner:
    def speak(): String = "Woof!"

  // A Cat class that extends the previous traits + overides two inherited methods
  class Cat(val name: String) extends Speaker, TailWagger, Runner:
    def speak(): String = "Yowl!"
    override def startRunning(): Unit = println("Yeah ... I do't run")
    override def stopRunning(): Unit = println("No need to stop")

  // Examples of the Dog and Cat class in Use
  val myDog = Dog("Socks")
  print(s"${myDog.name} goes, ")
  println(myDog.speak())      

  val myCat = Cat("Yowlie")
  print(s"${myCat.name} goes, ")
  println(myCat.speak())      
  myCat.startRunning()        
  myCat.stopRunning()         

  // Classes
  class Person(var firstName: String, var lastName: String):
    def printFullName() = println(s"$firstName $lastName")

  val personA = Person("Teddy", "McTedface")
  println(personA.firstName)   
  println(personA.lastName) 
  personA.lastName = "Tedteddy"
  personA.printFullName()      

  // FP Domain Modeling
  // Enumerations
  enum CrustSize:
    case Small, Medium, Large

  enum CrustType:
    case Thin, Thick, Regular

  enum Topping:
    case Cheese, Pepperoni, BlackOlives, GreenOlives, Onions

  import CrustSize.*
  val currentCrustSize = Small

  // enums in a `match` expression
  currentCrustSize match
    case Small => println("Small crust size")
    case Medium => println("Medium crust size")
    case Large => println("Large crust size")

  // enums in an `if` statement
  if currentCrustSize == Small then println("Small crust size")

  // how to create a sum type with Scala
  enum Nat:
    case Zero
    case Succ(pred: Nat)

  // Function to compute the successor of a Nat
  def successor(n: Nat): Nat = n match
    case Nat.Zero => Nat.Succ(Nat.Zero)
    case Nat.Succ(pred) => Nat.Succ(successor(pred))

  // Example usage
  val zero: Nat = Nat.Zero
  val one: Nat = Nat.Succ(zero)
  val two: Nat = successor(one)
  println(s"Zero: $zero")
  println(s"One: $one")
  println(s"Two: $two")

  // Product Types
  // case class feature
  // define a case class
  case class Persona(
    name: String,
    vocation: String
  )

  // create an instance of the case class
  val p = Persona("Reginald Kenneth Dwight", "Singer")

  println(p)                      // Persona(Reginald Kenneth Dwight,Singer)

  println(p.name)                 // "Reginald Kenneth Dwight"
  println(p.vocation)             // "Singer"
  // remove first // below to see what happens if you try to change a immutable field
  // p.name = "Joe"               // error: can’t reassign a val field

  // when you need to make a change, use the `copy` method to “update as you copy”
  val p2 = p.copy(name = "Elton John")
  println(p2)                     // : Person = Person(Elton John,Singer)
  println(p2.name)                 // "Reginald Kenneth Dwight"
  println(p2.vocation)             // "Singer"

