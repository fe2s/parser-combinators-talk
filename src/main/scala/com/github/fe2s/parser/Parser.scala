package com.github.fe2s.parser


import scala.annotation.tailrec
import Parser._

/**
 * @author Oleksiy_Dyagielv
 */

trait Parser[+T] extends (String => ParseResult[T]) {

  // step 1
  def ~>[U](right: => Parser[U]): Parser[U] = parser { input =>
    (this ~ right)(input) map { case (r1, r2) => r2 }
  }

  // step 2
  //    def ~>[U](right: => Parser[U]): Parser[U] = {
  //      this.flatMap(r1 => right)
  //    }

  // step 3
  //  def ~>[U](right: => Parser[U]): Parser[U] = {
  //    for (a <- this; b <- right) yield b
  //  }

  // step 1
  //    def <~ [U](right: => Parser[U]): Parser[T] = takeFirst(this, right)

  // step 3
  def <~[U](right: => Parser[U]): Parser[T] = {
    for (a <- this; b <- right) yield a
  }


  def *() = zeroToMany(this)

  def +() = oneToMany(this)

  def ~[U](right: => Parser[U]): Parser[(T, U)] = and(this, right)

  def |[U >: T](right: => Parser[U]): Parser[U] = parser { input =>
    this(input) match {
      case Failure(_) => right(input)
      case succ => succ
    }
  }

  def map[U](f: T => U): Parser[U] = parser { in => this(in) map f }

  def flatMap[U](f: T => Parser[U]): Parser[U] = parser { in => this(in) withNext f }

  def >>[U](f: T => U): Parser[U] = map(f)

  def rep1Sep[U](sep: Parser[U]): Parser[List[T]] = (this ~ (sep ~> this).*) map { case (x, xs) => x +: xs }

  def repSep[U](sep: Parser[U]): Parser[List[T]] = this.rep1Sep(sep) | success(List())


}


object Parser {

  def parser[T](f: String => ParseResult[T]) = new Parser[T] {
    override def apply(in: String): ParseResult[T] = f(in)
  }

  def runParser[T](input: String, parser: Parser[T]) = {
    val result = parser(input)
    result match {
      case Success(res, rest) => println(s"res = $res rest = $rest")
      case Failure(msg) => println(s"parser failed $msg")
    }
  }

  // combinators

  def iff[T](p: Parser[T], f: T => Boolean) = parser { input =>
    p(input) match {
      case succ@Success(res, rest) => if (f(res)) succ else Failure("iff failed")
      case failure => failure
    }
  }

  def zeroToMany[T](p: Parser[T]): Parser[List[T]] = parser { input =>
    @tailrec
    def parseInternal(current: Success[List[T]]): Success[List[T]] = {
      p(current.rest) match {
        case Success(res, rest) => parseInternal(Success(res +: current.result, rest))
        case _ => current
      }
    }
    parseInternal(Success(List(), input)).map(_.reverse)
  }

  def oneToMany[T](p: Parser[T]): Parser[List[T]] = parser { in =>
    zeroToMany(p)(in) match {
      case s@Success(list, rest) => if (list.isEmpty) Failure("at least one expected") else s
    }
  }

  def and[A, B](parserA: Parser[A], parserB: Parser[B]) = parser { input =>
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

  def or[A, B <: A](parserA: Parser[A], parserB: Parser[B]): Parser[A] = parser { input =>
    parserA(input) match {
      case s@Success(_, _) => s
      case _ => parserB(input)
    }
  }

  def repeat1Separated[A, B](parserA: Parser[A], parserB: Parser[B]): Parser[List[A]] = (parserA ~ (parserB ~> parserA).*) map { case (x, xs) => x +: xs }

  def success[T](res: T) = parser { in => Success(res, in) }

  //  def failure(msg:String) = parser {in => Failure(msg)}

  def repeatSeparated[A, B](parserA: Parser[A], parserB: Parser[B]): Parser[List[A]] = repeat1Separated(parserA, parserB) | success(List())


}