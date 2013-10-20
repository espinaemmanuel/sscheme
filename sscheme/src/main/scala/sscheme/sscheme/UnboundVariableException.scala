package sscheme.sscheme

class UnboundVariableException(val name : String) 
extends SSchemeException("'%s' is not bound to this environment".format(name)) {

}