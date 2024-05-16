object TrueRing {
  def rule = println("To rule them all")
}

object MainRing {
  def main(args: Array[String]): Unit = {
    TrueRing.rule // This calls the 'rule' method from outside the 'TrueRing' object
  }
}