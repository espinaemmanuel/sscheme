package sscheme.sscheme

import scala.collection.mutable.ArrayBuffer
import sscheme.sscheme.types.EMPTY_LIST
import sscheme.sscheme.types.vBoolean
import sscheme.sscheme.types.Value
import sscheme.sscheme.types.Pair


object ListUtils {
  
  def first(value : Value) : Value = {
    value match {
      case Pair(head, tail) => head
      case _ => throw new Exception("not a pair")
    }
  }
  
  def rest(value : Value) : Value = {
    value match {
      case Pair(head, tail) => tail
      case _ => throw new Exception("not a pair")
    }
  }
  
  def tail(v1 : Value) : Pair = v1 match {
    case Pair(head, tail : Pair) => tail
    case _ => throw new Exception("not a list")
  }
  
  def second(value : Value) : Value = first(rest(value))
  def cons(v1 : Value, v2 : Value) : Value = Pair(v1, v2)
  def third(x : Value) = first(rest(rest(x)))
  def list(v1 : Value, v2 : Value) = Pair(v1, Pair(v2, EMPTY_LIST))

  
  def truth(v : Value) = v match {
    case vBoolean(false) => false
    case _ => true
  }

}