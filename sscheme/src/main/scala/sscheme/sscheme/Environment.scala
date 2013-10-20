package sscheme.sscheme

import scala.collection.mutable.Map
import sscheme.sscheme.types.vSymbol
import sscheme.sscheme.types.Value

class Environment(val parent : Option[Environment]) {
  
  val internalMap : Map[String, Value] = Map()
  
  def lookup(name : String) : Value = internalMap.get(name) match {
    case Some(value) => value
    case None => parent match {
      case Some(p) => p.lookup(name)
      case None => throw new UnboundVariableException(name)
    }
  }
  
  def define(variable: vSymbol, value : Value) = {
    internalMap.put(variable.name, value)
    variable
  }

}