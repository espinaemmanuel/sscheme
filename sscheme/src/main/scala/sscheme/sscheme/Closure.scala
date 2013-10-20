package sscheme.sscheme

import sscheme.sscheme.types.Value


case class Closure(parms : Value, body : Value, env: Environment) extends Value {

}

