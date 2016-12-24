package commandeer


trait Foo {
  def bar(a: String, b: Int): Double
}

object Foo {
  implicit val fooDSL: FooDSL = new FooDSL {}

  trait FooDSL extends CommandeerDSL[Foo] {
    case class Bar(a: String, b: Int) extends Operation[Double]
  }
}


@generateDSL
trait Baz {
  def bash(a: String, b: Int): Double
  def imp(x: Int)(implicit ev: Int <:< Int): Int
}



object RunMe {
  def main(args: Array[String]): Unit = {
    println("Hi Mum")

    val kevin = CommandeerDSL(null.asInstanceOf[Foo])
    println(s"Found DSL for Foo: $kevin")
    val kevinBar = kevin.Bar("bob", 3)
    println(s"Made a bar: $kevinBar")

    val bob = CommandeerDSL(null.asInstanceOf[Baz])
    println(s"Found DSL for Baz: $bob")
    val bobBash = bob.bash("bob", 3)
    println(s"Made a bar: $bobBash")
    val bobImp = bob.imp(42)
    println(s"Made an imp: $bobImp")

    // doesn't compile -- the Operation type in the various DSLs are isolated.
//    val bobBashAsKevinOps : kevin.Operation[Double] = bobBash
  }
}

