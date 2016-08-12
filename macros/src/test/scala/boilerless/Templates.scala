package boilerless

object Templates {
  
  
  @enumInFile("core/src/test/scala/boilerless/gen", "boilerless.gen")
  class Opt[+T] { class Som[T](value: T); object Non }
  
  enumInFile("Opt2", "core/src/test/scala/boilerless/gen", "boilerless.gen")(){"""
    class Opt2[+T] {
      class Som[T](value: T)
      object Non
    }
  """}
  
  
  
}

