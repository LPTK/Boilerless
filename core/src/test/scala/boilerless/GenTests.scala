package boilerless

import org.scalatest.FunSuite

class GenTests extends FunSuite {
  import gen._
  
  def typed[T](x: T) = x
  
  test("Opt") {
    typed[Opt[Nothing]](Opt.Non)
    typed[Opt[Int]](Opt.Non)
    typed[Opt[Int]](Opt.Som(42))
  }
  
  test("Opt2") {
    typed[Opt2[Nothing]](Opt2.Non)
    typed[Opt2[Int]](Opt2.Non)
    typed[Opt2[Int]](Opt2.Som(42))
  }
  
  test("Misc") {
    
    val m = new Misc
    assert(new m.MemberClass().foo == 42)
    
    case class IDareYou() extends Misc.ExtensibleCase
    
    class M extends Misc
    
  }
  
}

