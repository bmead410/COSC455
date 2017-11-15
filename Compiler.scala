package edu.towson.cis.cosc455.bmead1.project1

import scala.io.Source

//import scala.io.Source
object Compiler{

  var currentToken : String = ""
  var fileContents : String = ""
  var isText : Boolean = false

  val Scanner = new MyLexicalAnalyzer
  val Parser = new MySyntaxAnalyzer
  val SemanticAnalyzer = new MySyntaxAnalyzer

  def main(args: Array[String]) = {
    // get input file name from command line argument
    val file = args(0)
    val fileContents = Source.fromFile(file).getLines.mkString
      checkFile(args)
      readFile(args(0))
    Scanner.startState(fileContents)
    Parser.gittex()
   // Scanner.getNextToken()

       /* while(Parser.validTokens != Nil) {
          //Scanner.getNextToken()
          //println(currentToken)
          println(Parser.validTokens)
        } */
    }
 // }
/*
    Method to read the file from command line?
    Takes in a file, and stores as String
    val are not changeable */

  def readFile(file : String) = {
    val source = scala.io.Source.fromFile(file)
    fileContents = try source.mkString finally source.close()
    //println(fileContents)
  }

  def checkFile(args : Array[String]) = {
    if (args.length != 1) {
      println("USAGE ERROR: wrong number of args!")
      System.exit(1)
    }
      //make sure to change back to .gtx
    else if (! args(0).endsWith(".gtx")) {
      println("USAGE ERROR: wrong extension!")
      System.exit(1)
    }
  }

  def isError() = ???
}
