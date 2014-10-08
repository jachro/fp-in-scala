package various

import java.lang.instrument.Instrumentation

import org.scalatest.{Matchers, WordSpec}
import sun.instrument.InstrumentationImpl
import various.Generics.{Sub1Sub1Class, Sub1Class, SuperClass}

class GenericsSpec extends WordSpec with Matchers {

  "Generics" should {
    "have methodEnsuringTwoArgTypes working" in {
      Generics.methodEnsuringTwoArgTypes[Sub1Class](new Sub1Class(), new Sub1Sub1Class())
    }
  }


  "" should {
    "" in {
      new InstrumentationImpl().getAllLoadedClasses.foreach(println(_))
    }
  }
}
