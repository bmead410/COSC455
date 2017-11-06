package edu.towson.cis.cosc455.bmead1.project1

/*
4 function signatures we must implement to have good design

 */
trait LexicalAnalyzer {

  def addChar(): Unit


  def getChar(): Char


  def getNextToken(): Unit


  def lookup(): Boolean


}