package various

object Generics {

  class SuperClass()
  class Sub1Class() extends SuperClass
  class Sub1Sub1Class() extends Sub1Class

  def methodEnsuringTwoArgTypes[A >: Sub1Class <: Sub1Class](arg1: A, arg2: A) = {
    println(s"$arg1 $arg2")
  }

}
