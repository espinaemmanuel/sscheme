package sscheme.sscheme.types

import scala.collection.mutable.Builder
import scala.collection.mutable.ListBuffer
import scala.collection.generic.CanBuildFrom
import scala.annotation.tailrec
import sscheme.sscheme.InvalidTypeException

case class Pair(first: Value, rest: Value) extends Value with Iterable[Value] {
     
  override def iterator = new Iterator[Value] {
    var current: Value = Pair.this

    override def hasNext = current != EMPTY_LIST

    override def next(): Value = current match {
      case Pair(currentVal, next) => {
        next match {
          case nextPair: Pair => current = nextPair
          case EMPTY_LIST => current = EMPTY_LIST
          case improperVal => current = Pair(improperVal, EMPTY_LIST)
        }
        currentVal
      }
      case _ => return EMPTY_LIST
    }
  }
  
  @tailrec final def isProper : Boolean = rest match {
    case EMPTY_LIST => true
    case next : Pair => next.isProper
    case _ => false
  }
  
  override def toString = {
    @tailrec def stringRepresentation(p: Pair, prefix : String) : String = {
      p.rest match {
        case EMPTY_LIST => prefix + " " + p.first
        case next : Pair => stringRepresentation(next, prefix + " " + p.first)
        case improperEnding => "%s %s . %s".format(prefix, p.first, p.rest)
      }
    }
    
    "(" + stringRepresentation(this, "").trim + ")";
  }

}

object Pair {
  
  import sscheme.sscheme.ListUtils._
  
  implicit def toPair(v : Value) : Pair = v match {
    case v : Pair => v
    case _ => throw new InvalidTypeException[Pair](v)
  }
  
  implicit object PairCBF extends CanBuildFrom[Iterable[Value], Value, Pair] {

    override def apply(from: Iterable[Value]) = new ListBuilder

    def apply() = new ListBuilder

  }

}


object EMPTY_LIST extends Value {
  override def toString = "()";
}