/* Generated automatically with Boilerless. Changes made directly to this file will be lost. */

package boilerless.gen

import boilerless._

sealed abstract class Opt2[+T]

object Opt2 {
  final case class Som[+T](value: T) extends Opt2[T];
  final case object Non extends Opt2[Nothing] with scala.Serializable with scala.Product {
    private[this] type T = Nothing
  }
}

