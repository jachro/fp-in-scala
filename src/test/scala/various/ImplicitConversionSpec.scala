package various

import org.scalatest.{Matchers, WordSpec}

class ImplicitConversionSpec extends WordSpec with Matchers {

  "returnConsideringGeneric" should {
    "return Sub1 if given Sub1" in {
      ImplicitConversion.returnConsideringGenericType(new Sub1 {}) should be ("Sub1")
    }
    "return Sub2 if given Sub2" in {
      ImplicitConversion.returnConsideringGenericType(new Sub2 {}) should be ("Sub2")
    }
  }
}
