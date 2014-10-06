package various

class Covariance {

  class SuperClass
  class Sub1Class extends SuperClass
  class Sub1Sub1Class extends Sub1Class

  class Wrap[+T <: Sub1Class]

//  def foo0(x: Wrap[SuperClass]): Wrap[SuperClass] = identity(x)
  def foo1(x: Wrap[Sub1Class]): Wrap[Sub1Class] = identity(x)

//  foo0(new Wrap[SuperClass])
//  foo0(new Wrap[Sub1Class])
//  foo0(new Wrap[SubSub1Class])

//  foo1(new Wrap[SuperClass])
  foo1(new Wrap[Sub1Class])
  foo1(new Wrap[Sub1Sub1Class])

  new Wrap[Sub1Class]
  new Wrap[Sub1Sub1Class]

  

  def foo2[T <: Sub1Class](x: Wrap[T]): Wrap[T] = identity(x)

//  foo2(new Wrap[SuperClass])
  foo2(new Wrap[Sub1Class])
  foo2(new Wrap[Sub1Sub1Class])
}
