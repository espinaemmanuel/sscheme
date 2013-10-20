package sscheme.sscheme

trait Token {

}

case class IdentifierToken(name: String) extends Token{
  
}

case class BooleanToken(value: Boolean) extends Token{
  
}

case class NumberToken(value: Double) extends Token{
  
}

case class CharacterToken(value: Char) extends Token{
  
}

case class StringToken(value: String) extends Token{
  
}

object LPAREN extends Token;
object RPAREN extends Token;
object EOF extends Token;
object QUOTE extends Token;
object BACKQUOTE extends Token;
object COMMA_AT extends Token;
object COMMA extends Token;
object SPACE extends Token;
object POUND_PAREN extends Token;
object DOT extends Token;