package boilerless

import org.scalatest.FunSuite

class BasicTests extends FunSuite {
  
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
  
  
  
}
