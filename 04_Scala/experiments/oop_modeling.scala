// Based on examples from https://docs.scala-lang.org/scala3/book/domain-modeling-oop.html

@main def oop() =
  // Traits
  trait ShowableAbstract:
    def show: String
  trait ShowableConcrete:
    def show: String
    def showHtml = "<p>" + show + "</p>"
  class Document(text: String) extends ShowableAbstract:
    def show = text
  /* 
  Abstract methods are not the only thing that can be left abstract in a trait. A trait can contain:
  - abstract methods (def m(): T)
  - abstract value definitions (val x: T)
  - abstract type members (type T), potentially with bounds (type T <: S)
  - abstract givens (given t: T)
  */

  // Mixin Composition
  trait GreetingService:
    def translate(text: String): String
    def sayHello = translate("Hello")

  trait TranslationService:
    def translate(text: String): String = "..."
    trait ComposedService extends GreetingService, TranslationService

  trait ComposedService extends GreetingService, TranslationService

  // Classes
  // Like traits, classes can extend multiple traits (but only one super class)
  class MyService(name: String) extends ComposedService, ShowableConcrete:
    def show = s"$name says $sayHello"
  
  // Subtyping - We can create an instance of MyService
  val s1: MyService = MyService("Service 1")
  // Through the means of subtyping, our instance s1 can be used everywhere that any of the extended traits is expected
  val s2: GreetingService = s1
  val s3: TranslationService = s1
  val s4: ShowableConcrete = s1
  
  // It is possible to extend another class
  class Person(name: String)
  class SoftwareDeveloper(name: String, favoriteLang: String)
      extends Person(name)
  
  // In Scala 3 extending non-abstract classes in other files is restricted. 
  // In order to allow this, the base class needs to be marked as open
  // open class Person(name: String)

  // Instances and Private Mutable State
  class Counter:
    // can only be observed by the method `count`
    private var currentCount = 0

    def tick(): Unit = currentCount += 1
    def count: Int = currentCount

  // in this example, Every instance of the class Counter has its own private state 
  // that can only be observed through the method count
  val c1 = Counter()
  println(c1.count)
  c1.tick()
  c1.tick()
  println(c1.count)

  // Advanced Example: Service Oriented Design
  // Adapted from the paper “Scalable Component Abstractions” by Martin Odersky and Matthias Zenger.
  // Define a trait for a subject-observer pattern
  trait SubjectObserver:
    // Define type aliases with upper type bounds
    type S <: Subject
    type O <: Observer

    // Define a trait for subjects that can be observed
    trait Subject:
      self: S => // Self-type; each Subject must also be an S
      private var observers: List[O] = List()       // A private list to hold observers
      def subscribe(obs: O): Unit =                 // Method to add an observer to the list
        observers = obs :: observers                // Prepend the observer to the list
      def publish(): Unit =                         // Notify all observers about an event
        for obs <- observers do obs.notify(this)    // Call notify on each observer

    // Define a trait for observers that can observe subjects
    trait Observer:
      def notify(sub: S): Unit                      // Method to be called when a subject publishes an event

  // Implement the SubjectObserver pattern for a sensor network
  object SensorReader extends SubjectObserver:
    // Specify the concrete types for S and O
    type S = Sensor
    type O = Display

    // Define a class for sensors
    class Sensor(val label: String) extends Subject:
      private var currentValue = 0.0                // Holds the current value of the sensor
      def value = currentValue                      // Getter for the current value
      def changeValue(v: Double): Unit =            // Setter for the current value
        currentValue = v                            // Update the current value
        publish()                                   // Notify all observers about the value change

    // Define a class for displays
    class Display extends Observer:
      def notify(sub: Sensor): Unit =                   // Implementation of the notify method
        println(s"${sub.label} has value ${sub.value}") // Print the sensor's label and value

  // Import members from the SensorReader object
  import SensorReader.*

  // Set up a network of sensors and displays
  val sen1 = Sensor("sensor1")                          // Create a sensor with label "sensor1"
  val sen2 = Sensor("sensor2")                          // Create another sensor with label "sensor2"
  val dis1 = Display()                                  // Create a display
  val dis2 = Display()                                  // Create another display
  sen1.subscribe(dis1)                                  // Subscribe display1 to sensor1
  sen1.subscribe(dis2)                                  // Subscribe display2 to sensor1
  sen2.subscribe(dis1)                                  // Subscribe display1 to sensor2

  // Propagate updates through the network by changing sensor values
  sen1.changeValue(2)                   // Change the value of sensor1 to 2.0 and notify observers
  sen2.changeValue(3)                   // Change the value of sensor2 to 3.0 and notify observers

  // The following lines will be printed as a result of the notifications:
  // sensor1 has value 2.0
  // sensor1 has value 2.0
  // sensor2 has value 3.0
