package sscheme.sscheme

import scala.collection.mutable.Builder
import scala.collection.mutable.ArrayBuffer
import sscheme.sscheme.ListUtils._
import scala.collection.immutable.Vector
import sscheme.sscheme.types.vChar
import sscheme.sscheme.types.vString
import sscheme.sscheme.types.vBoolean
import sscheme.sscheme.types.vNumber
import sscheme.sscheme.types.vVector
import sscheme.sscheme.types.vSymbol
import sscheme.sscheme.types.EMPTY_LIST
import sscheme.sscheme.types.Value
import sscheme.sscheme.types.Pair


class Parser(t: Tokenizer) {

  def readTail: Value = {
    var token = t.next;
    token match {
      case EOF => throw new Exception("EOF")
      case RPAREN => return EMPTY_LIST
      case DOT => {
        val result = read;
        token = t.next;
        if (token != RPAREN) throw new Exception("Where's the ')'? Got " + token + " after .");
        return result;
      }
      case _ => {
        t.isPushedToken = true;
        t.pushedToken = token;
        return Pair(read, readTail);
      }
    }
  }
  
  def toVector(value : Value) = value match {
    case list : Pair => Vector() ++ list
    case _ => Vector(value)
  }
  
  /**
   * Reads the next value
   */
  def read: Value = {

    val token = t.next;
    token match {
      case LPAREN => readTail
      case RPAREN => {
        println("Extra ) ignored.");
        return read
      }
      case POUND_PAREN => {
        t.pushedToken = LPAREN
        t.isPushedToken = true
        return vVector(toVector(read))
      }
      case DOT => {
        println("Extra . ignored.");
        return read
      }
      case QUOTE => return list(vSymbol("quote"), read)
      case BACKQUOTE => return list(vSymbol("quasiquote"), read)
      case COMMA => return list(vSymbol("unquote"), read)
      case COMMA_AT => return list(vSymbol("unquote-splicing"), read)
      case CharacterToken(v) => return vChar(v)
      case NumberToken(v) => return vNumber(v)
      case BooleanToken(v) => return vBoolean(v)
      case StringToken(v) => return vString(v)
      case IdentifierToken(v) => return vSymbol(v)

    }
  }

}