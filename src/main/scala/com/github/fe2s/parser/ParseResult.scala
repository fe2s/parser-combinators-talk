package com.github.fe2s.parser

/**
 * @author Oleksiy_Dyagilev
 */
sealed abstract class ParseResult[+T] {
  def map[U](f: T => U): ParseResult[U]
  def withNext[U](f: T => String => ParseResult[U]): ParseResult[U]
}

case class Success[+T](result: T, rest: String) extends ParseResult[T] {
  override def map[U](f: T => U): ParseResult[U] = Success(f(result), rest)
  override def withNext[U](f: T => String => ParseResult[U]) = f(result)(rest)
}

case class Failure(msg: String) extends ParseResult[Nothing] {
  override def map[U](f: Nothing => U) = this
  override def withNext[U](f: Nothing => String => ParseResult[U]) = this
}
