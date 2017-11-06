package edu.towson.cis.cosc455.bmead1.project1

object Compiler{

  var currentToken : String = ""
  var fileContents : String = ""

  val Scanner = new MyLexicalAnalyzer
  val Parser = new MySyntaxAnalyzer
  val SemanticAnalyzer = new MySyntaxAnalyzer

  def main(args: Array[String]): Unit = {
    // get input file name from command line argument
    val filename = args(0)
    val f = scala.io.Source.fromFile(filename).mkString
    Scanner.startState(f)
    checkFile(args)
    readFile(args(0))

    Scanner.getNextToken()

  }
/*
    Method to read the file from command line?
    Takes in a file, and stores as String
    val are not changeable
 */
  def readFile(file : String) = {
    val source = scala.io.Source.fromFile(file)
    fileContents = try source.mkString finally source.close()
  }

  def checkFile(args : Array[String]) = {
    if (args.length != 1) {
      println("USAGE ERROR: wrong number of args!")
      System.exit(1)
    }
      //make sure to change back to .gtx
    else if (! args(0).endsWith(".txt")) {
      println("USAGE ERROR: wrong extension!")
      System.exit(1)
    }
  }

  def isError() = ???
}
