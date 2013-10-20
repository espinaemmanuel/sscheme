package sscheme.sscheme

import sscheme.sscheme.types.Value

class NotApplicableException(val value : Value) 
extends SSchemeException("The object %s is not applicable".format(value)) {

}