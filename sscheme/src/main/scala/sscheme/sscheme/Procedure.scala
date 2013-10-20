package sscheme.sscheme

import sscheme.sscheme.types.Value
import sscheme.sscheme.types.Pair
import sscheme.sscheme.types.vNumber
import sscheme.sscheme.ListUtils._
import sscheme.sscheme.types.vNumber._

trait Procedure extends Value {  
  def apply(interpreter: SScheme, args : Pair) : Value

}

sealed trait ArgumentNumber
case class SpecificArgumentNumber(n : Int) extends ArgumentNumber
object UnboundedArguments extends ArgumentNumber

object SpecificArgumentNumber {
  implicit def toArgumentNumber(n : Int) = SpecificArgumentNumber(n)
}