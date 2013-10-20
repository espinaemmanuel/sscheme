package sscheme.sscheme

import sscheme.sscheme.types.Value
import sscheme.sscheme.types.Pair
import sscheme.sscheme.types.Pair._
import sscheme.sscheme.ListUtils._
import sscheme.sscheme.types.vSymbol._
import sscheme.sscheme.types.vSymbol

case class Closure(params : Value, body : Pair, parentEnv: Environment) extends Value {
  
  params match {
    case s : vSymbol =>
    case p : Pair =>
    case _ => throw new Exception("invalid formals list")
  }
  
  val _body = body.size  match {
    case 0 => throw new Exception("lambda body cannot be empty")
    case 1 => first(body)
    case _ => cons("begin", body)
    }

  def buildEnv(args: Value): Environment = {
    val lamdaEnv = new Environment(Some(parentEnv))
    params match {
      case variableArgs: vSymbol => lamdaEnv.define(variableArgs, params)
      case argsList: Pair =>
        if (argsList.isProper) 
          for((name, value) <- argsList.zip(args)){
            lamdaEnv.define(name, value)
          }
         else {
        	 null
        }
    }
    lamdaEnv
  }
  
  def executionContext(args : Pair) : Tuple2[Value, Environment] = (_body, buildEnv(args))
  
}

