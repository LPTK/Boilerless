package boilerless

import boilerless._

object Templates {
  
  
  @enumInFile("core/src/test/scala/boilerless/gen", "boilerless.gen")
  class Opt[+T] { class Som[T](value: T); object Non }
  
  
  enumInFile("Opt2", "core/src/test/scala/boilerless/gen", "boilerless.gen")(){"""
    class Opt2[+T] {
      class Som[T](value: T)
      object Non
    }
  """}
  
  
  @enumInFile("core/src/test/scala/boilerless/gen", "boilerless.gen")
  @open @concrete class Misc {
    @notCase @open abstract class ExtensibleCase(x: Unit)
    @ignore class MemberClass { def foo = 42 }
  }
  
}

