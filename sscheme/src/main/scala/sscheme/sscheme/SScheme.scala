package sscheme.sscheme

import scala.annotation.tailrec
import sscheme.sscheme.types.ListBuilder
import sscheme.sscheme.types.vNumber
import sscheme.sscheme.types.vSymbol
import sscheme.sscheme.types.EMPTY_LIST
import sscheme.sscheme.types.Value
import sscheme.sscheme.types.Pair
import sscheme.sscheme.ListUtils._
import sscheme.sscheme.types.vNumber

import scala.collection.generic.CanBuildFrom


class SScheme {

  def eval(value: Value, environment: Environment): Value = {
    sealed trait Bounce
    case class Done(result: Value) extends Bounce
    case class Call(thunk: Value, env: Environment) extends Bounce

    def doQuote(args: Value): Bounce = Done(first(args))

    def doBegin(args: Value, env: Environment): Bounce = {
      var current = args
      while (rest(current) != EMPTY_LIST) {
        eval(first(current), env)
        current = rest(current)
      }
      Call(first(current), env)
    }

    def doIf(args: Value, env: Environment): Bounce = if (truth(eval(first(args), env)))
      Call(second(args), env)
    else
      Call(third(args), env)
      
    def doDefine(args: Value, env: Environment) : Bounce = first(args) match {     
      case Pair(variable : vSymbol, formals) => {
        val lambdaExpression = cons(vSymbol("lambda"), cons(formals, rest(args)))
        Done(env.define(variable,  eval(lambdaExpression, env)))
      }
      case variable : vSymbol => Done(env.define(variable,  eval(second(args), env)))
      case _ => throw new Exception("invalid arguments for define")
    }
    
    def doLambda(args: Value, env: Environment) : Bounce = Done(Closure(first(args), rest(args), env))

    @tailrec def trampoline(bounce: Bounce): Value = bounce match {
      case Call(thunk, env) => trampoline(thunk match {
        case vSymbol(name) => Done(env.lookup(name))
        case Pair(fn, args) => fn match {
          case vSymbol("quote") => doQuote(args)
          case vSymbol("begin") => doBegin(args, env)
          case vSymbol("define") => doDefine(args, env)
          case vSymbol("lambda") => doLambda(args, env)
          case vSymbol("if") => doIf(args,env)
          case _ => eval(fn, env) match {
            case closure : Closure => Done(closure)
            case procedure: Procedure => args match {
              case args : Pair => Done(procedure.apply(args.map(arg => eval(arg, env)), env))
              case _ => throw new ProcedureCallException("Combination must be a proper list: %s".format(thunk))
            }
            case notApplicable => throw new NotApplicableException(notApplicable)
          }
        }
        case literal => Done(literal)
      })
      case Done(result) => result
    }

    trampoline(Call(value, environment))
  }
     
}

object SScheme {

  class Primitive(f: (Pair, Environment) => Value) extends Procedure {
    override def apply(args: Pair, env: Environment): Value = f(args, env)
  }

  implicit def functionToProcedure(f: (Pair, Environment) => Value) = new Primitive(f)
  implicit def numberToValue(v: Double) = vNumber(v)
}