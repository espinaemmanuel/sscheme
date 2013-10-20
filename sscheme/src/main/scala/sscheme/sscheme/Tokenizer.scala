package sscheme.sscheme

import java.io.Reader
import scala.collection.mutable.StringBuilder

class Tokenizer(in: Reader) {

  var isPushedToken = false;
  var isPushedChar = false;
  var pushedToken: Token = null;
  var pushedChar: Char = 1;

  def pushChar(ch: Char) {
    isPushedChar = true;
    pushedChar = ch;
  }

  def popChar(): Char = {
    isPushedChar = false;
    pushedChar;
  }

  def next: Token = {
    var ch: Char = 1

    if (isPushedToken) {
      isPushedToken = false;
      return pushedToken;
    } else if (isPushedChar) {
      ch = popChar();
    } else {
      ch = in.read().toChar;
    }

    while (Character.isWhitespace(ch))
      ch = in.read().toChar;

    ch match {
      case -1 => return EOF
      case '(' => return LPAREN
      case ')' => return RPAREN
      case '\'' => return QUOTE
      case '`' => return BACKQUOTE
      case ',' => {
        ch = in.read().toChar;
        if (ch == '@')
          return COMMA_AT;
        else {
          pushChar(ch);
          return COMMA;
        }
      }
      case ';' => {
        while (ch != -1.toChar && ch != '\n' && ch != '\r')
          ch = in.read().toChar;
        return next;
      }
      case '"' => {
        val buff = new StringBuilder
        ch = in.read().toChar;
        while (ch != '"' && ch != -1.toChar) {
          buff += (if (ch == '\\') in.read().toChar else ch);
          ch = in.read().toChar;
        }
        if (ch == -1)
          println("EOF inside of a string.");
        return StringToken(buff.toString())
      }
      case '#' => {
        ch = in.read().toChar
        ch match {
          case 't' | 'T' => return BooleanToken(true)
          case 'f' | 'F' => return BooleanToken(false)
          case '(' => return POUND_PAREN
          case '\\' => {
            	ch = in.read().toChar;
				if (ch == 's' || ch == 'S' || ch == 'n' || ch == 'N') {
					pushChar(ch);
					val token = next;
					token match {
					  case IdentifierToken("space") => return CharacterToken(' ')
					  case IdentifierToken("newline") => return CharacterToken('\n')
					  case _ => {
					    isPushedToken = true;
						pushedToken = token;
						return CharacterToken(ch.toChar);
					  }
					}
				} else {
					return CharacterToken(ch.toChar);
				}
          }

          case 'e' | 'i' | 'd' => next;
          case 'b' | 'o' | 'x' => {
            println("#" + ch + " not implemented, ignored.")
            return next;
          }
          case _ => {
            println("#" + ch + " not recognized, ignored.")
            return next;
          }
        }
      }
      case _ => {
        val buff = new StringBuilder;
        var c = ch;
        do {
          buff += ch;
          ch = in.read().toChar;
        } while (!ch.isWhitespace && ch != -1.toChar
          && ch != '(' && ch != ')' && ch != '\'' && ch != ';'
          && ch != '"' && ch != ',' && ch != '`');
        pushChar(ch);
        // Try potential numbers, but catch any format errors.
        if (c == '.' || c == '+' || c == '-' || (c >= '0' && c <= '9')) {
          parseDouble(buff.toString()) match {
            case Some(doubleVal) => return NumberToken(doubleVal)
            case None => if(buff.toString() == ".") return DOT
          }
        }
        
        return IdentifierToken(buff.toString().toLowerCase())
      }
    }
  }
  
  def parseDouble(s: String) = try { Some(s.toDouble) } catch { case _ => None }
}