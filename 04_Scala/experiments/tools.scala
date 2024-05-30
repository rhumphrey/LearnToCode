// Based on examples from https://docs.scala-lang.org/scala3/book/domain-modeling-tools.html

@main def tools() =
  // Classes
  class Person(var name: String, var vocation: String)
  class Book(var title: String, var author: String, var year: Int)
  class Movie(var name: String, var director: String, var year: Int)

  val p = Person("Vincent Damon Furnier", "Singer")

  println(p.name)
  println(p.vocation)

  // Because all the all of the parameters were created as var fields, 
  // I can so you can modify or 'mutate' them the are mutable if we wanted them
  // immutable then we would use val in the original class definition.
  p.name = "Alice Cooper"
  p.vocation = "Performer"

  println(p.name)
  println(p.vocation)

  // Fields and methods in classes
  class Servitor(var firstName: String, var lastName: String):

    println("initialization begins")
    val fullName = firstName + " " + lastName

    // a class method
    def printFullName: Unit =
    // access the `fullName` field, which is created above
    println(fullName)

    printFullName
    println("initialization ends")

  // the above definitions in use 
  val yowlie = Servitor("Yowlie", "Kitty")

  // Default parameters
  class Socket(val timeout: Int = 5_000, val linger: Int = 5_000):
    override def toString = s"timeout: $timeout, linger: $linger"
  // Use case for above default parameter example - notice where the defaults are used
  val s1 = Socket()                  // timeout: 5000, linger: 5000
  val s2 = Socket(2_500)             // timeout: 2500, linger: 5000
  val s3 = Socket(10_000, 10_000)    // timeout: 10000, linger: 10000
  val s4 = Socket(timeout = 10_000)  // timeout: 10000, linger: 5000
  val s5 = Socket(linger = 10_000)   // timeout: 5000, linger: 10000
  // Notice the option to use names parameters when creating a new instance
  // option 1 - not named
  val s6 = Socket(10_000, 10_000)
  // option 2 - named
  val s = Socket(
    timeout = 10_000,
    linger = 10_000
  )

  // Auxiliary constructors - you can define a class to have multiple constructors 
  // so consumers of your class can build it in different ways.
  import java.time.*

  // [1] the primary constructor
  class Student(
    var name: String,
    var govtId: String
  ):
    private var _applicationDate: Option[LocalDate] = None
    private var _studentId: Int = 0

    // [2] a constructor for when the student has completed
    // their application
    def this(
      name: String,
      govtId: String,
      applicationDate: LocalDate
    ) =
      this(name, govtId)
      _applicationDate = Some(applicationDate)

    // [3] a constructor for when the student is approved
    // and now has a student id
    def this(
      name: String,
      govtId: String,
      studentId: Int
    ) =
      this(name, govtId)
      _studentId = studentId
  // It can then be used like this
  val stu1 = Student("Mary", "123")
  val stu2 = Student("Mary", "123", LocalDate.now)
  val stu3 = Student("Mary", "123", 456)

  // Objects
  object StringUtils:
    def truncate(s: String, length: Int): String = s.take(length)
    def containsWhitespace(s: String): Boolean = s.matches(".*\\s.*")
    def isNullOrEmpty(s: String): Boolean = s == null || s.trim.isEmpty

  print(StringUtils.truncate("Socks The Corgi", 5))

  // Scala has very flexible importing
  // We can do all 
  import StringUtils.*
  truncate("Socks The Corgi", 5)       
  containsWhitespace("Yowlie Kitty")   
  isNullOrEmpty("Pink Mouse Dave")   
  // If we wanted to do some of the members it would look like the example in the comment below
  //  import StringUtils.{truncate, containsWhitespace}
  //  truncate("Socks The Corgi", 5)          // "Socks"
  //  containsWhitespace("Yowlie Kitty")      // true
  //  isNullOrEmpty("Pink Mouse Dave")        // Not found: isNullOrEmpty (error) 

  // Objects can also contain fields, which are also accessed like static members:
  object MathConstants:
    val PI = 3.14159
    val E = 2.71828
    val C = 299_792_458
  println(MathConstants.PI)   
  println(MathConstants.E)
  println(MathConstants.C)

  // Companion objects
  // An object that has the same name as a class, and is declared in the 
  // same file as the class, is called a “companion object.” 
  // Similarly, the corresponding class is called the object’s companion class. 
  // A companion class or object can access the private members of its companion.
  // Companion objects are used for methods and values that are 
  // not specific to instances of the companion class.
  import scala.math.*

  class Circle(val radius: Double):
    def area: Double = Circle.calculateArea(radius)

  object Circle:
    private def calculateArea(radius: Double): Double = Pi * pow(radius, 2.0)

  val circle1 = Circle(5.0)
  circle1.area

  // Other uses of companion objects
  // They can contain apply methods, which work as factory methods to construct new instances
  class Persona:
    var name = ""
    var age = 0
    override def toString = s"$name is $age years old"

  object Persona:
    // a one-arg factory method
    def apply(name: String): Persona =
      var p = new Persona
      p.name = name
      p


    // a two-arg factory method
    def apply(name: String, age: Int): Persona =
      var p = new Persona
      p.name = name
      p.age = age
      p

  end Persona

  val joe = Persona("Teddy")
  val fred = Persona("Grom", 29)

  // Traits
  // Traits can contain:
  // - Abstract methods and fields
  // - Concrete methods and fields
  
  // In a basic use, a trait can be used as an interface, 
  // defining only abstract members that will be implemented by other classes
  trait Employee:
    def id: Int
    def firstName: String
    def lastName: String

  // A trait can contain more concrete members
  trait HasLegs:
    def numLegs: Int
    def walk(): Unit
    def stop() = println("Stopped walking")

  // Another example
  trait HasTail:
    def tailColor: String
    def wagTail() = println("Tail is wagging")
    def stopTail() = println("Tail is stopped")

  // So, later in your code, classes can mix multiple traits to build larger components
  class Corgi(name: String) extends HasLegs, HasTail:
    val numLegs = 4
    val tailColor = "Sable"
    def walk() = println("I'm walking")
    override def toString = s"$name is a Dog"
  
  val doggoIs = Corgi("Socket")
  println(doggoIs)

  // Abstract classes - using trait parameters
  trait Pet(name: String):
    def greeting: String
    def age: Int
    override def toString = s"My name is $name, I say $greeting, and I'm $age"

  class Dog(name: String, var age: Int) extends Pet(name):
    val greeting = "Woof"

  val doggoSays = Dog("Socket", 1)
  println(doggoSays)

  // Enums 
  enum CrustSize:
    case Small, Medium, Large
  enum CrustType:
    case Thin, Thick, Regular
  enum Topping:
    case Cheese, Pepperoni, BlackOlives, GreenOlives, Onions

  // Enums in use
  // To use them in other code, first import them, and then use them
  import CrustSize.*
  val currentCrustSize = Large

  // Enum values can be compared using equals (==), and also matched on
  // if/then
  if currentCrustSize == Large then
    println("You get a big piece!")
  // match
  currentCrustSize match
    case Small => println("small")
    case Medium => println("medium")
    case Large => println("large")
  
  // Additional Enum Features
  // Enumerations can also be parameterized
  enum Color(val rgb: Int):
    case Red   extends Color(0xFF0000)
    case Green extends Color(0x00FF00)
    case Blue  extends Color(0x0000FF)

  // They can also have members (like fileds and methods)
  enum Planet(mass: Double, radius: Double):
    private final val G = 6.67300E-11
    def surfaceGravity = G * mass / (radius * radius)
    def surfaceWeight(otherMass: Double) =
      otherMass * surfaceGravity

    case Mercury extends Planet(3.303e+23, 2.4397e6)
    case Earth   extends Planet(5.976e+24, 6.37814e6)

  // The above in use
  // Create instances
  val mercury = Planet.Mercury
  val earth = Planet.Earth
  // Calculate surface gravity and weight
  val mercuryGravity = mercury.surfaceGravity
  val earthWeight = earth.surfaceWeight(75.0) // Assuming otherMass is 75 kg

  // Print results
  println(s"Mercury surface gravity: $mercuryGravity m/s^2")
  println(s"Earth surface weight for 75 kg: $earthWeight N")

  // Case classes
  // Case classes are used to model immutable data structures.
  case class Personage(name: String, relation: String)
  // We declared Personage as a case class, the fields name and relation are public and immutable by default. 
  // We can create instances of case classes as follows
  val erik = Personage("Erik", "uncle")
  // remember that the fields cant be mutated, doing so creates an error
  // erik.name = "Eric"   // error: reassignment to val

  // With immutable case classes scala gives you access to some useful methods, for example
  // Case classes can be used as patterns
  erik match
    case Personage(n, r) => println("Name is " + n)
  // `equals` and `hashCode` methods generated for you
  val noggin = Person("Noggin", "uncle")
  println(erik == noggin)                         
  // `toString` method
  println(erik)  
  // built-in `copy` method
  case class BaseballTeam(name: String, lastWorldSeriesWin: Int)
  val cubs1908 = BaseballTeam("Chicago Cubs", 1908)
  val cubs2016 = cubs1908.copy(lastWorldSeriesWin = 2016)

  // Case objects
  // Case objects are to objects what case classes are to classes: 
  // they provide a number of automatically-generated methods to make them more powerful. 
  // They’re particularly useful whenever you need a singleton object that needs a little extra functionality, 
  // such as being used with pattern matching in match expressions.
  // Case objects are useful when you need to pass immutable messages around. 
  sealed trait Message
  case class PlaySong(name: String) extends Message
  case class IncreaseVolume(amount: Int) extends Message
  case class DecreaseVolume(amount: Int) extends Message
  case object StopPlaying extends Message

  class MusicPlayer:
    private var currentVolume: Int = 50 // Initial volume

    def playSong(name: String): Unit =
      println(s"Playing song: $name")
      // Your actual implementation to play the song goes here

    def changeVolume(amount: Int): Unit = 
      currentVolume += amount
      println(s"Volume adjusted by $amount. Current volume: $currentVolume")
      // Your actual implementation to adjust volume goes here

    def stopPlayingSong(): Unit =
      println("Stopping playback.")
      // Your actual implementation to stop playback goes here

    def handleMessages(message: Message): Unit = message match
      case PlaySong(name)         => playSong(name)
      case IncreaseVolume(amount) => changeVolume(amount)
      case DecreaseVolume(amount) => changeVolume(-amount)
      case StopPlaying            => stopPlayingSong()

  // Example usage:
  val player = new MusicPlayer()
  player.handleMessages(PlaySong("Bohemian Rhapsody"))
  player.handleMessages(IncreaseVolume(10))
  player.handleMessages(DecreaseVolume(5))
  player.handleMessages(StopPlaying)