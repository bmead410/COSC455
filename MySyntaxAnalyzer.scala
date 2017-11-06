package edu.towson.cis.cosc455.bmead1.project1
/*
need to create an arrayList : ArrayList Created CHECK
need to use a while loop to add the accepted tokens to the stack
 */

class MySyntaxAnalyzer extends SyntaxAnalyzer{

  override var errorFound : Boolean = false
  def isError() = errorFound = true
  def getError : Boolean = errorFound
  var validTokens: List[String]   //stack for pushing accepted tokens onto the stack after each if statement

  def Text() = CONSTANTS.plainText //is this right?


  //<gittex> ::= DOCB <variable-define> <title> <body> DOCE
  override def gittex(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCB)){
      // add to parse tree / stack
      Compiler.Scanner.getNextToken()
    }
    else {
      println("Error")
      //System.exit(1) //commented out until run
    }
  }

  //<paragraph> ::= PARAB <variable-define> <inner-text> PARAE
  override def paragraph(): Unit = ???

 /*
    <inner-item> ::= <variable-use> <inner- item>
      | <bold> <inner- item>
        | <link> <inner- item>
          | REQTEXT <inner- item>
            | ε
  */
  override def innerItem(): Unit = ???

  /*
    <inner-text> ::= <variable-use> <inner-text>
| <heading> <inner-text>
| <bold> <inner-text>
| <listitem> <inner-text>
| <image> <inner-text>
| <link> <inner-text>
| TEXT <inner-text>
| ε
     */
  override def innerText(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB)) {
      variableUse()
      innerText()
    }

    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.HEADING)) {
      heading()
      innerText()
    }

    else if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD)) {
      bold()
      innerText()
    }

    else if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LISTITEM)) {
      listItem()
      innerText()
    }

    else if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.IMAGEB)){
      image()
      innerText()
    }

    else if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LINKB)){
      link()
      innerText()
    }

    else if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.TEXT)){ //not sure about this
      Text()
      innerText()
    }
}


  override def link(): Unit = ???

  override def italics(): Unit = ???

  /*<body> ::= <inner-text> <body>
       | <paragraph> <body>
       | <newline> <body>
       | ε
    */
  override def body(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB) || Compiler.currentToken.equalsIgnoreCase(CONSTANTS.HEADING)) {

    }
  }

      override def bold(): Unit = ???

  override def newline(): Unit = ???

    //<title> ::= TITLEB REQTEXT BRACKETE
  override def title(): Unit = ???

   // <variable-define> ::= DEFB REQTEXT EQSIGN REQTEXT BRACKETE <variable-define> | ε
  override def variableDefine(): Unit = ???

//<image> ::= IMAGEB REQTEXT BRACKETE ADDRESSB REQTEXT ADDRESSE | ε
    //Not sure if brackets here are correct or not...come back and check!!!
  override def image(): Unit =
    {
      if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.IMAGEB))
        {
          //add to stack
          Compiler.Scanner.getNextToken()

          if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.REQTEXT))
            {
              //add to stack
              Compiler.Scanner.getNextToken()

              if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE))
                {
                  //add to stack
                  Compiler.Scanner.getNextToken()

                  if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSB))
                    {
                      //add to stack
                      Compiler.Scanner.getNextToken()

                      if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.REQTEXT))
                       {
                         //add to stack
                         Compiler.Scanner.getNextToken()

                         if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSE))
                           {
                             //add to stack
                             Compiler.Scanner.getNextToken()
                           }
                         else {
                           println("Error")
                           System.exit(1)
                         }
                       }
                      else {
                        println("Error")
                        System.exit(1)
                      }

                    }
                  else {
                    println("Error")
                    System.exit(1)
                  }
                }
              else {
                println("Error")
                System.exit(1)
              }
            }
          else {
            println("Error")
            System.exit(1)
          }
        }
      else {
        println("Error")
        System.exit(1)
      }
    }


//not sure. Come back and check!!!!
  override def variableUse(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB))
    {
      //add to stack or array list
      Compiler.Scanner.getNextToken()

      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.REQTEXT))
      {
        //add to stack
        Compiler.Scanner.getNextToken()
        if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE))
        {
          //add to stack
          Compiler.Scanner.getNextToken()
        }
        else
        {
          println("Error")
          System.exit(1)
        }
      }
        else
        {
          //required text
          println("Error")
          System.exit(1)

        }
    }
    else
    {
      println("Error")
      System.exit(1)
    }

  }

    // <heading> ::= HEADING REQTEXT | ε
  override def heading(): Unit = ???

  override def listItem(): Unit = ???
}