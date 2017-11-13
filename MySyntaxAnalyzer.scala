package edu.towson.cis.cosc455.bmead1.project1
/*
need to create an arrayList : ArrayList Created CHECK
need to use a while loop to add the accepted tokens to the stack
 */
import scala.collection.mutable.Stack

class MySyntaxAnalyzer extends SyntaxAnalyzer{

  var errorFound : Boolean = false
  def isError() = errorFound = true
  def getError : Boolean = errorFound

  //stack for pushing accepted tokens onto the stack after each if statement
  var validTokens = Stack[String]()

  def Text() = CONSTANTS.plainText //is this right?


  /*
  <gittex> ::= DOCB <variable-define> <title> <body> DOCE
  --In progress
   */
  override def gittex(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCB)) { //this is my error boolean is false instead of true
      // add to parse tree / stack and call methods
      validTokens.push(Compiler.currentToken)
      variableDefine()
      title()
      body()
      //println(validTokens)

      //get following token
      Compiler.Scanner.getNextToken()

      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCE)) {
        validTokens.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
        println(validTokens)
      }
      else
      {
        println("Syntax Error: " + Compiler.currentToken)
        System.exit(1)
      }
    }
    else {
      println("Syntax Error at: " + Compiler.currentToken)
      System.exit(1)
    }
  }

  //<paragraph> ::= PARAB <variable-define> <inner-text> PARAE
  override def paragraph(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAB))
      {
        validTokens.push(Compiler.currentToken)
        variableDefine()
        innerText()
        if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAE))
          {
            //do I add to stack?
            validTokens.push(Compiler.currentToken)
            Compiler.Scanner.getNextToken()
          }
        else
          {
            println("Syntax Error at: " + Compiler.currentToken)
            System.exit(1)
          }
      }
    else
      {
        println("Syntax Error at: " + Compiler.currentToken)
        System.exit(1)
      }
  }

 /*
    <inner-item> ::= <variable-use> <inner- item>
      | <bold> <inner- item>
        | <link> <inner- item>
          | REQTEXT <inner- item>
            | ε
  */
  override def innerItem(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB)) {
      validTokens.push(Compiler.currentToken)
      variableUse()
      innerItem()
    }
    else if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD)) {
      validTokens.push(Compiler.currentToken)
      bold()
      innerItem()
    }
    else if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LINKB)) {
      validTokens.push(Compiler.currentToken)
      link()
      innerItem()
    }
      //not sure of this
    else if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.REQTEXT)) {
      validTokens.push(Compiler.currentToken)
      Text() //unsure here
      innerItem()
    }
  }

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
      validTokens.push(Compiler.currentToken)
      variableUse()
      innerText()
    }

    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.HEADING)) {
      validTokens.push(Compiler.currentToken)
      heading()
      innerText()
    }

    else if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD)) {
      validTokens.push(Compiler.currentToken)
      bold()
      innerText()
    }

    else if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LISTITEM)) {
      validTokens.push(Compiler.currentToken)
      listItem()
      innerText()
    }

    else if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.IMAGEB)){
      validTokens.push(Compiler.currentToken)
      image()
      innerText()
    }

    else if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LINKB)){
      validTokens.push(Compiler.currentToken)
      link()
      innerText()
    }

    else if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.TEXT)){ //not sure about this
      validTokens.push(Compiler.currentToken)
      Text()
      innerText()
    }
}

/*
<link> ::= LINKB REQTEXT BRACKETE ADDRESSB REQTEXT ADDRESSE | ε
 */
  override def link(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LINKB))
      {
        //add to stack
        validTokens.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()

        if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.REQTEXT))
          {
            //add to stack
            validTokens.push(Compiler.currentToken)
            Compiler.Scanner.getNextToken()

            if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE))
              {
                //add to stack
                validTokens.push(Compiler.currentToken)
                Compiler.Scanner.getNextToken()

                if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSB))
                  {
                    //add to stack
                    validTokens.push(Compiler.currentToken)
                    Compiler.Scanner.getNextToken()

                    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.REQTEXT))
                      {
                        //add to stack
                        validTokens.push(Compiler.currentToken)
                        Compiler.Scanner.getNextToken()

                        if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSE))
                          {
                            //add to stack
                            validTokens.push(Compiler.currentToken)
                            Compiler.Scanner.getNextToken()
                          }
                        else
                          {
                            println("Syntax Error: " + Compiler.currentToken)
                            System.exit(1)
                          }
                      }
                    else
                      {
                        println("Syntax Error:" + Compiler.currentToken)
                        System.exit(1)
                      }
                  }
                else
                  {
                    println("Syntax Error: " + Compiler.currentToken)
                    System.exit(1)
                  }

              }
            else
              {
                println("Syntax Error: " + Compiler.currentToken)
                System.exit(1)
              }
          }
        else
          {
            println("Syntax Error: " + Compiler.currentToken)
            System.exit(1)
          }
      }
    else
      {
        println("Syntax Error: " + Compiler.currentToken)
        System.exit(1)
      }

  }

//I don't think this method is in the guide
  //  override def italics(): Unit = ???
  /*
 <inner-text> ::= <variable-use> <inner-text>
| <heading> <inner-text>
| <bold> <inner-text>
| <listitem> <inner-text>
| <image> <inner-text>
| <link> <inner-text>
| TEXT <inner-text>
   */

  /*<body> ::= <inner-text> <body>
       | <paragraph> <body>
       | <newline> <body>
       | ε
    */
  override def body(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB) || Compiler.currentToken.equalsIgnoreCase(CONSTANTS.HEADING) ||
    Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD) || Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LISTITEM) ||
    Compiler.currentToken.equalsIgnoreCase(CONSTANTS.IMAGEB) || Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LINKB) ||
    Compiler.currentToken.equalsIgnoreCase(CONSTANTS.TEXT)) {
      //add to stack
      validTokens.push(Compiler.currentToken)
      innerText() //call inner-text
      body() //call body
      Compiler.Scanner.getNextToken() //get next token
    }
    else if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAB))
      {
        validTokens.push(Compiler.currentToken)
        paragraph()
        body()
        Compiler.Scanner.getNextToken() //get next token
      }
    else if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.NEWLINE))
      {
        newline()
        body()
        Compiler.Scanner.getNextToken() //get next token
      }
    else
      {
        println("Syntax Error: " + Compiler.currentToken)
        System.exit(1)
      }
  }

      /*
      <bold> ::= BOLD TEXT BOLD | ε
      do I push to stack at each if statement?
       */
      override def bold(): Unit = {
        if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD))
          {
            //push to stack
            validTokens.push(Compiler.currentToken)
            Compiler.Scanner.getNextToken()

            if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.TEXT))
            {
              //push to stack
              validTokens.push(Compiler.currentToken)
              Compiler.Scanner.getNextToken()

              if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD))
              {
                //push to stack
                validTokens.push(Compiler.currentToken)
                Compiler.Scanner.getNextToken()
              }
              else {
                println("Syntax Error: " + Compiler.currentToken)
                System.exit(1)
              }
            }
            else
              {
                println("Syntax Error: " + Compiler.currentToken)
                System.exit(1)
              }
          }
        else
          {
            println("Syntax Error: " + Compiler.currentToken)
            System.exit(1)
          }
      }

  /*
  <newline> ::= NEWLINE | ε
   */
  override def newline(): Unit =
  {
    if(Compiler.currentToken.equals(CONSTANTS.NEWLINE))
      {
        //push to stack
        validTokens.push(Compiler.currentToken)
        //get next token
        Compiler.Scanner.getNextToken()
      }
    else
      {
       println("Syntax Error: " + Compiler.currentToken)
       System.exit(1)
      }
  }

    //<title> ::= TITLEB REQTEXT BRACKETE
  override def title(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.TITLEB))
      {
        //push to stack
        validTokens.push(Compiler.currentToken)
        //get next token
        Compiler.Scanner.getNextToken()

        if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.REQTEXT))
          {
            //push to stack
            validTokens.push(Compiler.currentToken)
            //get next token
            Compiler.Scanner.getNextToken()

            if(Compiler.currentToken.equals(CONSTANTS.BRACKETE))
              {
                //push to stack
                validTokens.push(Compiler.currentToken)
                //get next token
                Compiler.Scanner.getNextToken()
              }
            else
              {
                println("Syntax Error: " + Compiler.currentToken)
                System.exit(1)
              }
          }
        else
          {
            println("Syntax Error: " + Compiler.currentToken)
            System.exit(1)
          }
      }
    else
      {
        println("Syntax Error: " + Compiler.currentToken)
        System.exit(1)
      }
  }

   // <variable-define> ::= DEFB REQTEXT EQSIGN REQTEXT BRACKETE <variable-define> | ε
  //add in the variable-define
  override def variableDefine(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DEFB))
      {
        //push to stack
        validTokens.push(Compiler.currentToken)
        //get next token
        Compiler.Scanner.getNextToken()

        if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.REQTEXT))
          {
            //push to stack
            validTokens.push(Compiler.currentToken)
            //get next token
            Compiler.Scanner.getNextToken()

            if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.EQSIGN))
              {
                //push to stack
                validTokens.push(Compiler.currentToken)
                //get next token
                Compiler.Scanner.getNextToken()

                if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.REQTEXT))
                  {
                    //push to stack
                    validTokens.push(Compiler.currentToken)
                    //get next token
                    Compiler.Scanner.getNextToken()

                    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE))
                      {
                        //push to stack
                        validTokens.push(Compiler.currentToken)
                        //get next token
                        Compiler.Scanner.getNextToken()
                        variableDefine()
                      }
                    else
                      {
                        println("Syntax Error: " + Compiler.currentToken)
                        System.exit(1)
                      }
                  }
                else
                  {
                    println("Syntax Error: " + Compiler.currentToken)
                    System.exit(1)
                  }
              }
            else
              {
                println("Syntax Error: " + Compiler.currentToken)
                System.exit(1)
              }
          }
        else
          {
            println("Syntax Error: " + Compiler.currentToken)
            System.exit(1)
          }
      }
    else
      {
        println("Syntax Error: " + Compiler.currentToken)
        System.exit(1)
      }
  }

//<image> ::= IMAGEB REQTEXT BRACKETE ADDRESSB REQTEXT ADDRESSE | ε
    //Not sure if brackets here are correct or not...come back and check!!!
  override def image(): Unit =
    {
      if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.IMAGEB))
        {
          //push to stack
          validTokens.push(Compiler.currentToken)
          //get next token
          Compiler.Scanner.getNextToken()

          if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.REQTEXT))
            {
              //push to stack
              validTokens.push(Compiler.currentToken)
              //get next token
              Compiler.Scanner.getNextToken()

              if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE))
                {
                  //push to stack
                  validTokens.push(Compiler.currentToken)
                  //get next token
                  Compiler.Scanner.getNextToken()

                  if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSB))
                    {
                      //push to stack
                      validTokens.push(Compiler.currentToken)
                      //get next token
                      Compiler.Scanner.getNextToken()

                      if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.REQTEXT))
                       {
                         //push to stack
                         validTokens.push(Compiler.currentToken)
                         //get next token
                         Compiler.Scanner.getNextToken()

                         if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSE))
                           {
                             //push to stack
                             validTokens.push(Compiler.currentToken)
                             //get next token
                             Compiler.Scanner.getNextToken()
                           }
                         else {
                           println("Syntax Error: " + Compiler.currentToken)
                           System.exit(1)
                         }
                       }
                      else {
                        println("Syntax Error: " + Compiler.currentToken)
                        System.exit(1)
                      }

                    }
                  else {
                    println("Syntax Error: " + Compiler.currentToken)
                    System.exit(1)
                  }
                }
              else {
                println("Syntax Error: " + Compiler.currentToken)
                System.exit(1)
              }
            }
          else {
            println("Syntax Error: " + Compiler.currentToken)
            System.exit(1)
          }
        }
      else {
        println("Syntax Error: " + Compiler.currentToken)
        System.exit(1)
      }
    }


//not sure. Come back and check!!!!
  /*
  <variable-use> ::= USEB REQTEXT BRACKETE | ε
   */
  override def variableUse(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB))
    {
      //push to stack
      validTokens.push(Compiler.currentToken)
      //get next token
      Compiler.Scanner.getNextToken()

      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.REQTEXT))
      {
        //push to stack
        validTokens.push(Compiler.currentToken)
        //get next token
        Compiler.Scanner.getNextToken()

        if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE))
        {
          //push to stack
          validTokens.push(Compiler.currentToken)
          //get next token
          Compiler.Scanner.getNextToken()
        }
        else
        {
          println("Syntax Error: " + Compiler.currentToken)
          System.exit(1)
        }
      }
        else
        {
          //required text
          println("Syntax Error: " + Compiler.currentToken)
          System.exit(1)
        }
    }
    else
    {
      println("Syntax Error: " + Compiler.currentToken)
      System.exit(1)
    }
  }

    // <heading> ::= HEADING REQTEXT | ε
  override def heading(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.HEADING))
      {
        //push to stack
        validTokens.push(Compiler.currentToken)
        //get next token
        Compiler.Scanner.getNextToken()

        if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.REQTEXT))
          {
            //push to stack
            validTokens.push(Compiler.currentToken)
            //get next token
            Compiler.Scanner.getNextToken()
          }
        else
        {
          println("Syntax Error: " + Compiler.currentToken)
          System.exit(1)
        }
      }
    else
    {
      println("Syntax Error: " + Compiler.currentToken)
      System.exit(1)
    }
  }

  /*
  <listitem> ::= LISTITEMB <inner-item> <list-item> | ε

  <inner-item> ::= <variable-use> <inner- item>
| <bold> <inner- item>
| <link> <inner- item>
| REQTEXT <inner- item>
| ε

<listitem> ::= LISTITEM <inner-item> <list-item> | ε

this is using recursion?
recalls listitem...how to implement this?
   */
  override def listItem(): Unit = {
    if (Compiler.currentToken.equals(CONSTANTS.LISTITEM)) {

      //push to stack
      validTokens.push(Compiler.currentToken)
      //get next token
      Compiler.Scanner.getNextToken()

      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB) || Compiler.currentToken.equals(CONSTANTS.BOLD) ||
        Compiler.currentToken.equals(CONSTANTS.LINKB) || Compiler.currentToken.equalsIgnoreCase(CONSTANTS.REQTEXT)) {

        //push to stack
        validTokens.push(Compiler.currentToken)
        //get next token
        Compiler.Scanner.getNextToken()
      }
      else
      {
        println("Syntax Error: " + Compiler.currentToken)
        System.exit(1)
      }
    }
    else
    {
      println("Syntax Error: " + Compiler.currentToken)
      System.exit(1)
    }
  }
}
