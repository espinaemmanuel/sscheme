package sscheme.sscheme.types

import scala.collection.mutable.Builder
import scala.collection.mutable.ListBuffer

class ListBuilder extends Builder[Value, Pair]{
  
  val b = new ListBuffer[Value]
  
  override def +=(elem: Value) = {b += elem; this}
  override def clear() = b.clear
  override def result() : Pair = {
    if(b.size == 0) throw new Exception("empty list")
    var last : Value = EMPTY_LIST
    for(v <- b.reverse){
      last = Pair(v, last)
    }
    last.asInstanceOf[Pair]
  }
}