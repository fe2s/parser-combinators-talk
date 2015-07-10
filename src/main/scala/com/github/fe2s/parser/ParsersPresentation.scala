package com.github.fe2s.parser

import com.github.fe2s.parser.Parser._
import com.github.fe2s.parser._

import scala.annotation.tailrec


/**
 * Snippets to be placed in presentation slides
 *
 * @author Oleksiy_Dyagilev
 */
object ParsersPresentation extends App {

  sealed abstract class ParseResult[+T] {
    def map[U](f: T => U): ParseResult[U]
    def withNext[U](f: T => String => ParseResult[U]): ParseResult[U]
  }

  case class Success[+T](result: T, rest: String) extends ParseResult[T] {
    override def map[U](f: (T) => U): ParseResult[U] = Success(f(result), rest)
    override def withNext[U](f: T => String => ParseResult[U]) = f(result)(rest)
  }

  case class Failure(msg: String) extends ParseResult[Nothing] {
    override def map[U](f: (Nothing) => U): ParseResult[U] = this
    override def withNext[U](f: Nothing => String => ParseResult[U]) = this
  }

  //  trait Parser[+T] {
  //    def parse(input:String): ParseResult[T]
  //  }


  //  trait Parser[+T] extends Function1[String, ParseResult[T]]
  trait Parser[+T] extends (String => ParseResult[T]) {

    def map[U](f: T => U): Parser[U] = parser { in => this(in) map f }
    def flatMap[U](f: T => Parser[U]): Parser[U] = parser { in => this(in) withNext f }

    // step 1
    //    def ~>[U](right: => Parser[U]): Parser[U] = parser { input =>
    //      and(this, right)(input) map { case (r1, r2) => r2 }
    //    }

//     step 2
//    def ~>[U](right: Parser[U]): Parser[U] = {
//      this.flatMap(r1 => right)
//    }

    // step 3
    def ~>[U](right: => Parser[U]): Parser[U] = {
      for (l <- this; r <- right) yield r
    }

    def <~[U](right: => Parser[U]): Parser[T] = {
      for (l <- this; r <- right) yield l
    }


    def ~[U](right: => Parser[U]): Parser[(T, U)] = and(this, right)
    def *() = many(this)
  }


  //  val charParser = new Parser[Char] {
  //    def apply(input: String): ParseResult[Char] = {
  //      if (input.isEmpty) Failure("string is empty") else Success(input.head, input.tail)
  //    }
  //  }

  def parser[T](f: String => ParseResult[T]) = new Parser[T] {
    def apply(in: String): ParseResult[T] = f(in)
  }

  val anyChar = parser { input =>
    if (input.isEmpty) Failure("string is empty") else Success(input.head, input.tail)
  }

  def many[T](p: Parser[T]): Parser[List[T]] = parser { input =>
    @tailrec
    def parseInternal(current: Success[List[T]]): Success[List[T]] = {
      p(current.rest) match {
        case Success(res, rest) => parseInternal(Success(res +: current.result, rest))
        case _ => current
      }
    }
    parseInternal(Success(List(), input)) map {
      _.reverse
    }
  }

  val stringParser: Parser[List[Char]] = many(anyChar)

  def iff[T](p: Parser[T], f: T => Boolean) = parser { input =>
    p(input) match {
      case succ@Success(res, rest) => if (f(res)) succ else Failure("iff failed")
      case failure => failure
    }
  }

  def char(c: Char): Parser[Char] = iff[Char](anyChar, _ == c)

  def charNot(except: Char*): Parser[Char] = iff[Char](anyChar, c => !except.contains(c))


  val quote = char('"')
  val digit = iff[Char](anyChar, _.isDigit)

  def and[A, B](parserA: Parser[A], parserB: Parser[B]): Parser[(A, B)] = parser { input =>
    parserA(input) match {
      case Success(res, rest) => parserB(rest) match {
        case Success(res2, rest2) => Success((res, res2), rest2)
        case _ => Failure(s"(and) second failed on $rest")
      }
      case _ => Failure("(and) first failed")
    }
  }

  def takeSecond[A, B](parserA: Parser[A], parserB: Parser[B]) = parser { input =>
    and(parserA, parserB)(input) map { case (r1, r2) => r2 }
  }

  def takeFirst[A, B](parserA: Parser[A], parserB: Parser[B]) = parser { input =>
    and(parserA, parserB)(input) map { case (r1, r2) => r1 }
  }


  println(anyChar("abc"))
  println(anyChar(""))

  println(stringParser("abc"))

  println(quote( """  "firstName"  """.trim))

  println(and(digit, char('a'))("3abc"))
  println(and(digit, char('a'))("3333"))

  //  val stringVal =
  //    takeFirst(
  //      takeSecond(
  //        quote,
  //        many(charNot('"'))
  //      ),
  //      quote
  //    )
  //

  //  val stringVal = char('"') ~> charNot('"').* <~ char('"')

  implicit def charToParser(c: Char): Parser[Char] = char(c)

  val stringVal = '"' ~> charNot('"').* <~ '"'

  println(stringVal( """  "firstName" """.trim))


}