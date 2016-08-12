package boilerless

import org.scalatest.FunSuite

class BasicTests extends FunSuite {
  
  test("Unsealed and NotFinal") {
    
    import Models._
    
    class Test0 extends Enum0
    
    assertDoesNotCompile("class Test1 extends Enum1") // Error:(12, 25) illegal inheritance from sealed class Enum1
    
    assertDoesNotCompile("class Test1 extends Enum.Case0") // Error:(13, 30) illegal inheritance from final class Case0
    
    class Test2 extends Enum0.Case1
    
  }
  
  
  
  
}
