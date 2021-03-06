package edu.towson.cis.cosc455.bmead1.project1
  /*
  This class will extend trait LexicalAnalyzer
  Lexical analyzer will scan in order to simplify the
  task of the parser, by reducing the size of the input

  "\" is replaced with "\\"
  "\\" is replaced with "\\\\"

  var (s) are mutable or changeable
  val (s) are immutable or not changeable

  QUESTIONS, COMMENTS, LOGIC, RUMORS
  -nested if? line 164

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
    val ReqText = List(CONSTANTS.plainText)

    /*
    implement a Start State
    Takes single line of text in form of sentence
    and gets first lexeme/token
    */
    def startState(line: String): Unit = {
      //initialize lexemes from Syntax -> not sure about this
      initializeKeyword()
      //get token from compiler
      sourceLine = line
      position = 0
      getChar()
      getNextToken()
    } //end startState

   /*
	This method adds the current character
     */
    override def addChar(): Unit = {

        lexLength += 1
        lexeme = nextChar :: lexeme

    }

    //return current character
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
	  DEFINITELY NEED TO DIFFERENTIATE BETWEEN
	  LETTERS, DIGITS, and OTHER SPECIAL CHARACTERS
     */
    override def getNextToken(): Unit = {
      lexLength = 0
      if(nextChar.isWhitespace) {
        skipWhiteSpace()
      }
      if(keyword.contains(nextChar)) {
        /*
            use methods for keywords
            Backslash method is common in the
            defined constants
        */
        if (nextChar == '\\') {
          processBS()
        }
        else if (nextChar == '!') {
          processImage()
        }
        else {
          processSymbols()
        }
        newToken = lexeme.reverse.mkString
        if(lookup(newToken)){
          Compiler.currentToken = newToken
          //println(newToken)
        }
      }

      else if (!nextChar.isWhitespace)
        {
          processText()
          Compiler.currentToken = lexeme.reverse.mkString
          Compiler.isText = true
        }
      else
      {
        //some sort of error
        println("Lexical error: please check for typos, misprints, etc.")
        System.exit(1)
      }
      lexeme = List()
    }

    /*process backslash char there are 8 constants that have BS
    BS means Backslash
    keep collecting elements until space char
    Still not printing first 2 tokens

    */
    def processBS(): Unit ={
          while(!nextChar.isWhitespace && nextChar != '[')
            {
              addChar()
              getChar()
            }
          if(nextChar == '[')
            {
              addChar()
              getChar()
            }
    }
    /*
    process heading char
    symbols are processed until reached a space char or keyword
     */
    def processSymbols() : Unit = {
      addChar()
      getChar()
    }

    //This works to my knowledge
    def processImage() : Unit = {
      if(nextChar == '!') {
        addChar()
        getChar()
      } //should this be a nested if?
        if (nextChar == '[') {
          addChar()
          getChar()
        }
      else
          {
            println("Lexical Error: " + nextChar + ". Looking for [ to complete Image definition.")
          }
    }

    //process text
    //This works to my knowledge
    def processText(): Unit = {
      //iterates until not a kw, or special space char
      //change to include Compiler.isText
          while(!keyword.contains(nextChar) && nextChar != '\n' && nextChar!= '\t' )
          {
            addChar()
            getChar()
          }
    }
/*
    Initializes all keywords that will be needed
 */
    def initializeKeyword() = {
      lexemeList = List("\\BEGIN", "\\END","\\TITLE[", "]","#","\\PARAB","\\PARAE","*","+",
        "\\", "[","(", ")","!", "\\DEF[", "=", "\\USE[")
    }

//added equalsIgnoreCase, hoping it works.
     override def lookup(candidateToken: String): Boolean = {
        if(!lexemeList.contains(candidateToken) && Compiler.currentToken.equalsIgnoreCase(candidateToken))
        {
            println("Lexical Error: " + candidateToken + " not an allowed keyword")
            return false
        }
       Compiler.isText = false
      return true
    }

   def skipWhiteSpace() : Unit = {
      while (nextChar.isWhitespace){
        getChar()
      }
    }
  }