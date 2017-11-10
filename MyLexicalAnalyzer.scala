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
    val letterList = List("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m",
      "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z")

    /*
    implement a Start State
    Takes single line of text in form of sentence
    and gets first lexeme/token
    */
    def startState(line: String): Unit = {
      //initialize lexemes from Syntax -> not sure about this
      initializeKeyword()
      //println(initializeKeyword())
      //println(lexemeList)
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

        lexLength += 1
        lexeme = nextChar :: lexeme

    }

    override def getChar(): Char = {
      if(position < sourceLine.length())
      {
        //
        nextChar = sourceLine.charAt(position)
        position +=1
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
      var pos2 = 1

      if(nextChar.isSpaceChar) {
        lexLength = 0
        skipWhiteSpace()
      }
      else if(keyword.contains(nextChar))
        {

            //use methods for keywords
          if(nextChar == '\\')
            {
              processBS()
            }
          else if(nextChar == '#'&& sourceLine.charAt(pos2).isSpaceChar) //not sure if this works
            {
              processHeading()
            }
          else if(nextChar == '*')
            {
              processBold()
            }
          else if(nextChar == '+')
            {
              processLI()
            }
          else if(nextChar == '[')
            {
              processLB()
            }
          else if(nextChar == '(')
            {
              processAB()
            }
          else if(nextChar == ')')
            {
              processAE()
            }
          else if(nextChar == '!')
            {
              processIB()
            }
          else if(nextChar == '=')
            {
              processEQ()
            }
          /*else if(nextChar == CONSTANTS.REQTEXT)
            {
              processText()
            } */

        }


      else
      {
        //some sort of error
        println("Lexical error: please check for typos, misprints, etc.")
      }
      lexeme = List()
    }

      /*
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
    keep collecting elements until not letter
    nextChar.toString.equalsIgnoreCase(letterList.mkString) &&
    keeps reading the \, we want to skip over this character. but
    still make sure it is a valid token
    */
    def processBS(): Unit ={
          while(!nextChar.isWhitespace && nextChar != '\n' )
            {
              addChar()
              getChar()
            }
          newToken = lexeme.reverse.mkString
          if(lookup(newToken)){
            Compiler.currentToken = newToken
          }
      println("The Lexical Analyzer has processed: " + newToken)
    }
    /*
    process heading char
    heading is # followed by space followed by text
     */
    def processHeading(): Unit = {

          while(nextChar.toString.equalsIgnoreCase(CONSTANTS.REQTEXT))
          {
            addChar()
            getChar()
          }
      newToken = lexeme.reverse.mkString
      println(newToken)
      //initializeKeyword()
      if(lookup(newToken)){
        Compiler.currentToken = newToken
      }
      println(newToken)
    }

    //process bold char
    def processBold(): Unit = {



    }

    //process list item char
    def processLI(): Unit = {

          while(nextChar != '\n' && nextChar != ' ')
          {
            addChar()
            getChar()
          }

    }

    //process linkB char
    def processLB(): Unit = {

          while(nextChar != '\n' && nextChar != ' ')
          {
            addChar()
            getChar()
          }

    }

    //process addressBegin char
    def processAB(): Unit = {
          while(nextChar != '\n' && nextChar != ' ')
          {
            addChar()
            getChar()
          }
    }

    //process addressEnd char
    def processAE(): Unit = {

          while(nextChar != '\n' && nextChar != ' ')
          {
            addChar()
            getChar()
          }

    }

    //process imageBegin char
    def processIB(): Unit = {

          while(nextChar != '\n' && nextChar != ' ')
          {
            addChar()
            getChar()
          }

    }

    //process equal char
    def processEQ(): Unit = {

          while(nextChar != '\n' && nextChar != ' ')
          {
            addChar()
            getChar()
          }

    }

    //process text
    def processText(): Unit = {
          while(nextChar != '\n' && nextChar != ' ')
          {
            addChar()
            getChar()
          }
    }
/*

 */
    def initializeKeyword() = {
      lexemeList = List("\\BEGIN", "\\END","\\TITLE[", "]","#","\\PARAB","\\PARAE","*","+",
        "\\", "[","(", ")","![", "\\DEF[", "=", "\\USE[")
    }

//debug
     override def lookup(candidateToken: String): Boolean = {
        if(!lexemeList.contains(candidateToken))
        {
            println("Lexical Error: " + candidateToken + " not an appropriate keyword")
            return false
        }
      return true
    }

    /*
    add legal keywords to the language
     */



    def skipWhiteSpace() : Unit = {
      while (nextChar.isSpaceChar){
        getChar()
      }
    }

    def initializeAcceptedLex() = {
      lexems = List(CONSTANTS.DOCB, CONSTANTS.DOCE, CONSTANTS.TITLEB, CONSTANTS.BRACKETE, CONSTANTS.HEADING,
        CONSTANTS.PARAB, CONSTANTS.PARAE,CONSTANTS.BOLD, CONSTANTS.LISTITEM, CONSTANTS.NEWLINE, CONSTANTS.LINKB,
        CONSTANTS.ADDRESSB, CONSTANTS.ADDRESSE, CONSTANTS.IMAGEB, CONSTANTS.DEFB, CONSTANTS.EQSIGN, CONSTANTS.USEB)
    //lexems = List("\\BEGIN")
    }

   // def isALetter(c: Char) = c.isLetter && c <= 'z'
  }