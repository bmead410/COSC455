package edu.towson.cis.cosc455.bmead1.project1
  /*
  This class will extend trait LexicalAnalyzer
  Lexical analyzer will scan in order to simplify the
  task of the parser, by reducing the size of the input

  "\" is replaced with "\\"
  "\\" is replaced with "\\\\"

  var (s) are mutable or changeable
  val (s) are immutable or not changeable

  QUESTIONS :

  1.

  2.
   */
  class MyLexicalAnalyzer extends LexicalAnalyzer {

    var sourceLine: String

    //list for lexemes
    var lexeme: List[Char]

    //placeholder for next character
    var nextChar: Char

    //
    var lexLength: Int

    //store position
    var position: Int

    //store lexemes in a string list
    var lexemeList: List[String]

    /*
    implement a Start State
    Takes single line of text in form of sentence
    and gets first lexeme/token
    */
    def startState(line: String): Unit = {
      //initialize lexemes from Syntax
      Compiler.Parser.gittex()
      //get token from compiler
      sourceLine = line
      position = 0
      getChar()
      getNextToken()
    } //end startState


    /*
   /*
	This method adds the current character
	the the token after checking to make
	sure that the length of the token
	isn't too long, a lexical error in this
	case.
	*/

     */
    override def addChar(): Unit = {
      if(lexLength <= 98){
        lexeme(lexLength.+(1)) == nextChar
        lexeme(lexLength) == 0
      }
      else {
        println("Lexical Error: The found lexeme is too long")

        if(!nextChar.isSpaceChar) {
          while(!nextChar.isSpaceChar){
            getChar()
          }
        }

        lexLength = 0
        skipWhiteSpace()
        addChar()

      }

    }


    /*
    This method does a character-by-character analysis to
    get the next token and set it in the Compiler
	  class's currentToken global String variable.
	  This simple lexical analyzer does not differentiate
	  between letters, digits and other special
	  characters - it simply looks for characters, spaces and
	  end of line characters to determine relevant tokens.

	  PROBABLY NEED TO DIFFERENTIATE BETWEEN
	  LETTERS, DIGITS, and OTHER SPECIAL CHARACTERS
     */
    override def getNextToken(): Unit = {
      lexLength = 0

      //We want to skip any white space
      skipWhiteSpace()

      addChar()
      getChar()


      //keep getting the characters for the token
      //add to stack
      //get next char
      while (nextChar.!=('\n') && nextChar.!=(' ')) {
        addChar()
        getChar()
      }
    }

    //Convert now gathered character array tokens into
    // a string
    var newToken: List[String]
    newToken :: lexeme

    /*
    Used slice for "substring".
    may or may not work?
    also is .eq used correctly?
    I set the current token equal to the element in the list
    newToken
     */
    if (lookup(newToken.slice(0, lexLength))) {
      Compiler.currentToken.eq(newToken.slice(0, lexLength))
    }


    /*
    What do we want to do here?
    if the position is before the end of the source,
    the
     */
    override def getChar(): Char = {
        if(position < sourceLine.length())
          {
            nextChar = sourceLine.charAt(position +1)
            //position +1
          }
        else {
          nextChar = '\n'
        }
    }


//why do I keep getting this error that lookup overrides nothing?
    override def lookup(candidateToken: List[String]): Boolean = {
        if(!lexemeList.contains(candidateToken)){
          Compiler.Parser.isError()
          println("Lexical Error: " )
          println(candidateToken)
          println(" not accepted.")
          return false
        }
        else {
          return true
        }
    }
    /*
       def validToken(ch : Char) : Boolean = {
         //correct

         I think what we want to do here is

          if(Compiler.currentToken.
    } */

    def skipWhiteSpace() : Unit = {
      while (nextChar.isSpaceChar){
        getChar()
      }
    }

   /* def isSpace(ch: Char) = Char {
      return ch.
    } */


  }