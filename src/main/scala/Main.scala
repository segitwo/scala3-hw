import CompletionArg.{ShowItIsFloat, ShowItIsInt, ShowItIsString}

extension (s1: String)
  //Не могу переопределить метод +
  def +:(s2: String): Int =
    val s3 = s1 + s2
    try
      s3.toInt
    catch
      case e: NumberFormatException => throw RuntimeException(s"Parameters of method +: must be a string of numbers")


enum CompletionArg:
  case ShowItIsString(s: String)
  case ShowItIsInt(i: Int)
  case ShowItIsFloat(f: Float)

object CompletionArg:
  given fromString: Conversion[String, CompletionArg] = ShowItIsString(_)

  given fromInt: Conversion[Int, CompletionArg] = ShowItIsInt(_)

  given fromFloat: Conversion[Float, CompletionArg] = ShowItIsFloat(_)
end CompletionArg

def complete[T](arg: CompletionArg) = arg match
  case ShowItIsInt(i) => println(s"It is int: $i")
  case ShowItIsString(s) => println(s"It is string: '$s'")
  case ShowItIsFloat(f) => println(s"It is float: $f")


object MyMath:
  opaque type Logarithm = Double

  object Logarithm:
    def apply(d: Double): Logarithm = math.log(d)

    def safe(d: Double): Option[Logarithm] =
      if d > 0.0 then Some(math.log(d)) else None
  end Logarithm

  // Методы расширения определяют общедоступные API непрозрачных типов.
  extension (x: Logarithm)

    def toDouble: Double = math.exp(x)

    def +(y: Logarithm): Logarithm = Logarithm(math.exp(x) + math.exp(y))

    def *(y: Logarithm): Logarithm = x + y

end MyMath

object Main

@main def run(): Unit =
  println("56" +: "3")

  complete("Simple string")
  complete(0.1f)
  complete(1)

  val l = MyMath.Logarithm(1.0)
  val l2 = MyMath.Logarithm(2.0)
  val l3 = l * l2
  val l4 = l + l2
  println(l3)
  println(l4)

