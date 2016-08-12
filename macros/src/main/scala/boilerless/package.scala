import scala.language.experimental.macros
import scala.annotation.{StaticAnnotation, compileTimeOnly}

/*
  TODO: make `enumeratum` an option and allow it in enumInFile  
*/
/** Macros used to define @enum classes. */
package object boilerless {
  
  /** To annotate each case class/object with custom options */
  class options(args: Any*) extends StaticAnnotation
  
  @compileTimeOnly("Enable macro paradise to expand macro annotations.")
  class enum(args: Any*) extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro Macros.impl
  }
  @compileTimeOnly("Enable macro paradise to expand macro annotations.")
  class enumeratum(args: Any*) extends StaticAnnotation { // TODO enumeratumToFile
    def macroTransform(annottees: Any*): Any = macro Macros.enumeratumImpl
  }
  
  @compileTimeOnly("Enable macro paradise to expand macro annotations.")
  class enumInFile(folderName: String, packageName: String)(args: Any*) extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro Macros.implInFile
  }
  
  def enumInFile(className: String, folderName: String, packageName: String)(args: Any*)(code: String): Unit = macro Macros.defEnumInFileImpl
  
  
}














