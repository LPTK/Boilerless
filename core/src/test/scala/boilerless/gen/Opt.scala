/* Generated automatically with Boilerless. Changes made directly to this file will be lost. */

package boilerless.gen


sealed abstract class Opt[+T]

object Opt {
  final case class Som[T](value: T) extends Opt[T];
  final case object Non extends Opt[Nothing] with scala.Serializable with scala.Product
}

