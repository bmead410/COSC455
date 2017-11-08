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

    var sourceLine: String = ""

    //list for lexemes
    var lexeme: List[Char] = List()

    //placeholder for next character
    var nextChar: Char = ' '

    //store token length
    var lexLength: Int = 0

    //store position
    var position: Int = 0

    //store lexemes in a string list
    var lexemeList: List[String] = List()
    var lexems: List[String] = List()
    var newToken: String = ""

    val keyword = List('!', '#', '\\','*','+','[', ']', '(', ')', '=')

    /*
    implement a Start State
    Takes single line of text in form of sentence
    and gets first lexeme/token
    */
    def startState(line: String): Unit = {
      //initialize lexemes from Syntax -> not sure about this
      //initializeAcceptedLex()
      //Compiler.Parser.gittex()
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
      if(lexLength <= 500){
        lexLength += 1
        lexeme(lexLength) == nextChar
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

    override def getChar(): Char = {
      if(position < sourceLine.length())
      {
        //trying to set the next character equal to the
        //sourceLine character at the next position
        nextChar = sourceLine.charAt(position +1)
        //position +1
        return nextChar
      }

      else {
        //nextChar = '\n'
        return nextChar
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

	  DEFINITELY NEED TO DIFFERENTIATE BETWEEN
	  LETTERS, DIGITS, and OTHER SPECIAL CHARACTERS
     */
    override def getNextToken(): Unit = {

      if(nextChar.isSpaceChar) {
        lexLength = 0
        skipWhiteSpace()
      }
      else if(keyword.contains(nextChar))
        {

            //use methods for keywords
          processBS()
         /* processBE()
          processHeading()
          processBold()
          processLI()
          processLB()
          processAB()
          processAE()
          processIB()
          processEQ()
          processText() */

        }
      else
      {
        //some sort of error
        println("Lexical error: please check for typos, misprints, etc.")
      }
    }




      /*
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
      //not sure if this is right yet
      Compiler.currentToken = newToken.slice(0, lexLength).toString()

    } */

    /*process backslash char there are 8 constants that have BS
    BS means Backslash
    */
    def processBS(): Unit ={

      if(nextChar == '\\')
        {
          while(nextChar != '\n' && nextChar != ' ')
            {
          addChar()
          getChar()
            }
          newToken = lexeme.toString()
          if(lookup(newToken.substring(0, lexLength))){
            Compiler.currentToken == newToken.substring(0, lexLength)
            println(newToken)
          }
        }
    }

    //process bracket end char
    def processBE(): Unit = {
      if(nextChar == ']')
        {
          while(nextChar != '\n' && nextChar != ' ')
          {
            addChar()
            getChar()
          }
        }
    }

    //process heading char
    def processHeading(): Unit = {
      if(nextChar == '#')
        {
          while(nextChar != '\n' && nextChar != ' ')
          {
            addChar()
            getChar()
          }
        }
    }

    //process bold char
    def processBold(): Unit = {
      if(nextChar == '*')
      {
        while(nextChar != '\n' && nextChar != ' ')
        {
          addChar()
          getChar()
        }
      }
    }

    //process list item char
    def processLI(): Unit = {
      if(nextChar == '+')
        {
          while(nextChar != '\n' && nextChar != ' ')
          {
            addChar()
            getChar()
          }
        }
    }

    //process linkB char
    def processLB(): Unit = {
      if(nextChar == '[')
        {
          while(nextChar != '\n' && nextChar != ' ')
          {
            addChar()
            getChar()
          }
        }
    }

    //process addressBegin char
    def processAB(): Unit = {
      if(nextChar == '(')
        {
          while(nextChar != '\n' && nextChar != ' ')
          {
            addChar()
            getChar()
          }
        }
    }

    //process addressEnd char
    def processAE(): Unit = {
      if(nextChar == ')')
        {
          while(nextChar != '\n' && nextChar != ' ')
          {
            addChar()
            getChar()
          }
        }
    }

    //process imageBegin char
    def processIB(): Unit = {
      if(nextChar == '!')
        {
          while(nextChar != '\n' && nextChar != ' ')
          {
            addChar()
            getChar()
          }
        }
    }

    //process equal char
    def processEQ(): Unit = {
      if(nextChar == '=')
        {
          while(nextChar != '\n' && nextChar != ' ')
          {
            addChar()
            getChar()
          }
        }
    }

    //process text
    def processText(): Unit = {
      if(nextChar == CONSTANTS.REQTEXT)
        {
          while(nextChar != '\n' && nextChar != ' ')
          {
            addChar()
            getChar()
          }
        }
    }


//why do I keep getting this error that lookup overrides nothing?
     override def lookup(candidateToken: String): Boolean = {
        if(!lexemeList.contains(candidateToken)){
          if(!CONSTANTS.plainText.contains(candidateToken)) {

            Compiler.Parser.isError()
            println("Lexical Error: " + candidateToken + " not accepted plaintext.")
            return false
          }
          else{
            println("Lexical Error: " + candidateToken + " not an appropriate keyword")
            return false
          }
        }
      return true
    }

    /*
    add legal keywords to the language
     */
    def initializeKeyword(candidateToken: List[String]) = {
        lexemeList = List(CONSTANTS.DOCB, CONSTANTS.DOCE, CONSTANTS.TITLEB, CONSTANTS.BRACKETE, CONSTANTS.HEADING,
         CONSTANTS.PARAB, CONSTANTS.PARAE,CONSTANTS.BOLD, CONSTANTS.LISTITEM, CONSTANTS.NEWLINE, CONSTANTS.LINKB,
          CONSTANTS.ADDRESSB, CONSTANTS.ADDRESSE, CONSTANTS.IMAGEB, CONSTANTS.DEFB, CONSTANTS.EQSIGN, CONSTANTS.USEB)
    }


    def skipWhiteSpace() : Unit = {
      while (nextChar.isSpaceChar){
        getChar()
      }
    }

    def initializeAcceptedLex() = {
      /*lexems = List(CONSTANTS.DOCB, CONSTANTS.DOCE, CONSTANTS.TITLEB, CONSTANTS.BRACKETE, CONSTANTS.HEADING,
        CONSTANTS.PARAB, CONSTANTS.PARAE,CONSTANTS.BOLD, CONSTANTS.LISTITEM, CONSTANTS.NEWLINE, CONSTANTS.LINKB,
        CONSTANTS.ADDRESSB, CONSTANTS.ADDRESSE, CONSTANTS.IMAGEB, CONSTANTS.DEFB, CONSTANTS.EQSIGN, CONSTANTS.USEB) */
    lexems = List("\\BEGIN")
    }

  }