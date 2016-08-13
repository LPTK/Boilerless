package boilerless

import org.scalatest.FunSuite

class BasicTests extends FunSuite {
  
  test("Enum with def and val") {
    
    @enum class Enum[A] {
      def foo(a: A): A
      val n: Int
      
      object X       { _[Int];    foo(a) = a;    n = 0 }
      object A       { _[Int];    foo(a) = 0;    n = 1 }
      
      class Y(x: Int){ _[String]; foo(a) = a;    n = x }
      class B(x: Int){ _[String]; foo(a) = "ok"; n = 3 }
    }
    import Enum._
    
    assert(X.n == 0)
    assert(Y(2).n == 2)
    
    assert(A.foo(42) == 0)
    assert(Y(0).foo("ko") == "ko")
    assert(B(0).foo("ko") == "ok")
    
  }
  
  
  test("Unsealed and NotFinal") {
    
    import Models._
    
    class Test0 extends Enum0
    
    assertDoesNotCompile("class Test1 extends Enum1") // Error:(12, 25) illegal inheritance from sealed class Enum1
    
    assertDoesNotCompile("class Test1 extends Enum.Case0") // Error:(13, 30) illegal inheritance from final class Case0
    
    class Test2 extends Enum0.Case1
    
    class Test3 extends Enum0.Case2
    
  }
  
  
  test("Nested") {
    
    @enum('NotInterested) class Level0(x: Int) {
      val z = x
      
      class Sub0(){0}
      class Sub1(x: Int){x}
      
      @enum class Level1(x: Int) { _(x)
        object SubSub0{1}
        class SubSub1(y: Int){y}
      }
    }
    
    assert(Level0.Sub0().z == 0)
    assert(Level0.Level1.SubSub0.x == 1)
    assert(Level0.Level1.SubSub1(42).x == 42)
    assert(Level0.Level1.SubSub1(42).y == 42)
    
  }
  
  
  test("Concrete Abstract Trick") {
    
    @enum class Enum {
      @concrete abstract def f = 0 // this is actually useless now
      def g = "ko"
      
      object A { f = 1 ; g = "ok" }
      object B
    }
    assert(Enum.A.f == 1 && Enum.B.f == 0)
    assert(Enum.A.g == "ok" && Enum.B.g == "ko")
    
  }
  
  
}




