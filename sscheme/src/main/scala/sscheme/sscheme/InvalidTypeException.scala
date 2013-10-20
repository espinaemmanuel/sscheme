package sscheme.sscheme

import sscheme.sscheme.types.Value

class InvalidTypeException[T](val v: Value)(implicit m: Manifest[T]) 
	extends SSchemeException("the object %s is of type %s. Expected : %s".format(v, v.typeName, m)) {
}