package various

sealed trait Super {
}

trait Sub1 extends Super {
}

trait Sub2 extends Super {
}

object ImplicitConversion {

//  def returnConsideringGenericType[A <: Super](v: A): String = {
//    v convert
//  }

  private implicit class SuperConverter(v: Super) {
    def convert = "Super"
  }

  def returnConsideringGenericType[A <: Sub1](v: A): String = {
    v convert
  }

  def returnConsideringGenericType(v: Sub2): String = {
    v convert
  }

  private implicit class Sub1Converter(v: Sub1) {
    def convert = "Sub1"
  }

  private implicit class Sub2Converter(v: Sub2) {
    def convert = "Sub2"
  }

}
