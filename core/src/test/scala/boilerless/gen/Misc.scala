/* Generated automatically with Boilerless. Changes made directly to this file will be lost. */

package boilerless.gen

import boilerless._

@open @concrete sealed abstract class Misc {
  class MemberClass {
    def foo = 42
  }
}

object Misc {
  @notCase @open final abstract case class ExtensibleCase() extends Misc
}

