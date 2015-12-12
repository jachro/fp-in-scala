package various

trait Converter[T] {
  def convert(s: String): T
}

object Converter {
  implicit val stringConverter = new Converter[String] {
    override def convert(s: String): String = s
  }

  implicit val intConverter = new Converter[Int] {
    override def convert(s: String): Int = s.toInt
  }

  implicit def someWrapperConverter[T](implicit converter: Converter[T]) = new Converter[SomeWrapper[T]] {
    override def convert(s: String): SomeWrapper[T] = SomeWrapper[T](converter.convert(s))
  }
}

case class SomeWrapper[T](s: T)

case class MagicBox(s: String) {
  def get[T](implicit converter: Converter[T]): T = converter.convert(s)
}
