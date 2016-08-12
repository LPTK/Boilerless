package boilerless

import org.scalatest.FunSuite
import enumeratum._

class EnumeratumTests extends FunSuite {
  
  
  test("States, Vanilla") {
    
    sealed abstract class State(override val entryName: String) extends EnumEntry
    object State extends Enum[State] {
       val values = findValues
       case object Alabama extends State("AL")
       case object Alaska extends State("AK")
       // and so on and so forth.
    }
    import State._
    
    assert(State.withName("AL") == Alabama)
    assert(State.withName("AK") == Alaska)
    
  }
  
  test("States, Boilerless @enum") {
    
    @enum class State(override val entryName: String) extends EnumEntry {
       object Alabama{"AL"}
       object Alaska{"AK"}
       // and so on and so forth.
    }
    object State extends Enum[State] { val values = findValues }
    import State._
    
    assert(State.withName("AL") == Alabama)
    assert(State.withName("AK") == Alaska)
    
  }
  
  test("States, Boilerless @enumeratum") {
    
    @enumeratum class State(entryName: String) {
       object Alabama{"AL"}
       object Alaska{"AK"}
       // and so on and so forth.
    }
    import State._
    
    assert(State.withName("AL") == Alabama)
    assert(State.withName("AK") == Alaska)
    
  }
  
  
  
  test("Dummy, Vanilla") {
    
    sealed trait DummyEnum extends EnumEntry
    object DummyEnum extends Enum[DummyEnum] {
      case object Hello extends DummyEnum
      case object GoodBye extends DummyEnum
      case object Hi extends DummyEnum
      val values = findValues
    }
    import DummyEnum._
    
    assert(DummyEnum.withName("Hello") == Hello)
    assert(DummyEnum.withName("GoodBye") == GoodBye)
    assert(DummyEnum.withName("Hi") == Hi)    
    
  }
  
  test("Dummy, Boilerless @enum") {
    
    @enum class DummyEnum extends EnumEntry {
      object Hello
      object GoodBye
      object Hi
    }
    object DummyEnum extends Enum[DummyEnum] {
      val values = findValues
    }
    import DummyEnum._
    
    assert(DummyEnum.withName("Hello") == Hello)
    assert(DummyEnum.withName("GoodBye") == GoodBye)
    assert(DummyEnum.withName("Hi") == Hi)    
    
  }
  
  
  test("Dummy, Boilerless @enumeratum") {
    
    @enumeratum class DummyEnum {
      object Hello
      object GoodBye
      object Hi
    }
    import DummyEnum._
    
    assert(DummyEnum.withName("Hello") == Hello)
    assert(DummyEnum.withName("GoodBye") == GoodBye)
    assert(DummyEnum.withName("Hi") == Hi)    
    
  }
  
  
  
  
  
  
}






