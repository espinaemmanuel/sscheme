package sscheme.sscheme

import jline.console.ConsoleReader
import sscheme.sscheme.types.vSymbol
import sscheme.sscheme.types.Pair
import sscheme.sscheme.types.Value
import sscheme.sscheme.types.vNumber
import java.io.StringReader


object Repl extends App {
    val console = new ConsoleReader
    console setPrompt "> "
    console println "Enter a line of text (type 'quit' to exit): "
    
    val env = BaseEnvironment       
    val scheme = new SScheme()   

    var curLine = console.readLine
    while (curLine != "exit") {
      val parser = new Parser(new Tokenizer(new StringReader(curLine)))
      val value = parser.read
      try {
        	val result = scheme.eval(value, env);
        	console println (result.typeName + ": " + result)
      } catch {
      	case e : SSchemeException => console.println(e.getClass().getName() + ": " + e.getMessage())
      }

      curLine = console.readLine()
    }

}