package edu.towson.cis.cosc455.bmead1.project1

  /*
  12 functions given by Professor Dehlinger


    // For ease, store the terminal literals in a List

   QUESTIONS:
   1. how do we add the or symbol
   2. how to we add the empty value
   3.

   */

  trait SyntaxAnalyzer {

    var errorFound : Boolean = false


    def gittex() : Unit


    def title() : Unit
    //def validTokens() : Unit
    def body() : Unit
    def paragraph() : Unit
    def innerText() : Unit

    def heading() :Unit

    def variableDefine() : Unit
    def variableUse() : Unit
    def bold() : Unit
    def italics() : Unit
    def listItem() : Unit
    def innerItem() : Unit
    def link() : Unit
    def image() : Unit
    def newline() : Unit
  }
