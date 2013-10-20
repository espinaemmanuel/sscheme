package sscheme.sscheme

object BaseEnvironment extends Environment(None) {
  definePrimitive(Plus)
  definePrimitive(Times)
  definePrimitive(Max)
  definePrimitive(Min)
  definePrimitive(Minus)
  definePrimitive(Div)
  definePrimitive(Abs)

  def definePrimitive(p: Primitive) = define(p.op, p)
}