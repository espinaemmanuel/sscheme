package sscheme.sscheme;

import sscheme.sscheme.types.Pair
import sscheme.sscheme.types.vNumber
import sscheme.sscheme.types.Value
import sscheme.sscheme.ListUtils._
import sscheme.sscheme.types.vNumber._
import sscheme.sscheme.SpecificArgumentNumber._
import sscheme.sscheme.types.vBoolean
import sscheme.sscheme.types.vBoolean

abstract class Primitive(val op : String, val minArgs : ArgumentNumber, val maxArgs : ArgumentNumber) extends Procedure {
	  
    def apply(args : Pair, env : Environment) : Value = {
      val argsSize = args.size
      minArgs match {
        case SpecificArgumentNumber(n) => if(argsSize < n) throw new ProcedureCallException("The procedure %s has been called with %d arguments; it requires at least %d arguments".format(this, argsSize, n))
        case UnboundedArguments =>
      }
      
      maxArgs match {
        case SpecificArgumentNumber(n) => if(argsSize > n) throw new ProcedureCallException("The procedure %s has been called with %d arguments; it requires at most %d arguments".format(this, argsSize, n))
        case UnboundedArguments =>
      }
      doApply(args, env)
    }
    
    def doApply(args : Pair, env : Environment) : Value

}

class NumberReduce(op : String, fn: (vNumber, vNumber) => vNumber) extends Primitive(op, 0, UnboundedArguments){
    override def doApply(args : Pair, env : Environment) : Value = tail(args).foldLeft(toNumber(first(args)))((acum, v)=>fn(acum, v))
}

object Plus extends NumberReduce("+", (a, b) => a + b) { }
object Times extends NumberReduce("*", (a, b) => a * b) { } 
object Max extends NumberReduce("max", (a, b) => Math.max(a.value, b.value)) { } 
object Min extends NumberReduce("min", (a, b) => Math.min(a.value, b.value)) { } 

object Minus extends Primitive("-", 1, UnboundedArguments) { 
  override def doApply(args : Pair, env : Environment) : Value = args.size match {
        case 1 => vNumber(0) - first(args)
        case list => tail(args).foldLeft(toNumber(first(args)))((acum, v) => acum - v)
      }
}

object Div extends Primitive("/", 1, UnboundedArguments) { 
  override def doApply(args : Pair, env : Environment) : Value = args.size match {
        case 1 => vNumber(1) / first(args)
        case list => tail(args).foldLeft(toNumber(first(args)))((acum, v) => acum / v)
      }
}

class BooleanPrimitive(op : String, fn : (vNumber, vNumber) => Boolean) extends  Primitive(op, 2, UnboundedArguments){
  override def doApply(args : Pair, env : Environment) : Value = {
    var current : vNumber = first(args)
    for(v <- tail(args)){
      if(!fn(current, v)) {
        return vBoolean(false)
      } else {
        current = v
      }
    }
    vBoolean(true)
  }
}

object Lt extends BooleanPrimitive("<", (a, b) => a < b) {}
object Le extends BooleanPrimitive("<=", (a, b) => a <= b) {}
object Eq extends BooleanPrimitive("=", (a, b) => a == b) {}
object Gt extends BooleanPrimitive(">", (a, b) => a > b) {}
object Ge extends BooleanPrimitive(">=", (a, b) => a >= b) {}

object Abs  extends Primitive("abs", 1, 1){
    override def doApply(args : Pair, env : Environment) : Value = Math.abs(toNumber(first(args)).value)
}