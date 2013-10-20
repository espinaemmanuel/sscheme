package sscheme.sscheme.types

import scala.collection.mutable.Builder
import scala.collection.mutable.ListBuffer
import sscheme.sscheme.InvalidTypeException

trait Value {
  
  def typeName = this.getClass.getSimpleName

}

case class vVector(vector : IndexedSeq[Value]) extends Value{
  
}

case class vSymbol(name : String) extends Value{
  override def toString = name;
}

object vSymbol{
  implicit def stringToSymbol(name : String) = vSymbol(name)
}

case class vNumber(value : Double) extends Value {
  
  override def toString = if(value.isValidInt) value.toInt.toString else value.toString;
  
  def +(other: vNumber) = vNumber(this.value + other.value)
  def -(other: vNumber) = vNumber(this.value - other.value)
  def *(other: vNumber) = vNumber(this.value * other.value)
  def /(other: vNumber) = vNumber(this.value / other.value)
  def <(other: vNumber) = this.value < other.value
  def >(other: vNumber) = this.value > other.value
  def <=(other: vNumber) = this.value <= other.value
  def >=(other: vNumber) = this.value >= other.value
  def ==(other: vNumber) = this.value == other.value
  

}

object vNumber {
  implicit def toNumber(v : Value) : vNumber = v match {
    case v : vNumber => v
    case _ => throw new InvalidTypeException[vNumber](v)
  }
  
  implicit def doubleToNumber(v : Double) = vNumber(v) 
}

case class vBoolean(value: Boolean) extends Value {
  
}

case class vChar(value: Char) extends Value {
  
}

case class vString(value: String) extends Value {
  
  override def toString = "\"" + value + "\"";
  
}
