package com.github.fe2s.parser

import Parser._

/**
 * @author Oleksiy_Dyagilev
 */
trait CharParser {

  val char: Parser[Char] = parser { input =>
    if (input.isEmpty) Failure("string is empty") else Success(input.head, input.tail)
  }

  def str(s:String): Parser[String] = parser { input =>
    input.indexOf(s) match {
      case 0 => Success(s, input.drop(s.length))
      case _ => Failure(s"input $input doesn' start with string $s")
    }
  }

  implicit def charToParser(c:Char): Parser[Char] = char(c)
  implicit def stringToParser(s:String): Parser[String] = str(s)

  def char(c: Char): Parser[Char] = iff[Char](char, _ == c)

  val digit: Parser[Char] = iff[Char](char, _.isDigit)

  def charNot(except: Char*): Parser[Char] = iff[Char](char, c => !except.contains(c))

  val intNumber: Parser[Int] = digit.+ map {charList => charList.mkString.toInt}

}
