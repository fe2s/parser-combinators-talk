package com.github.fe2s.parser

import com.github.fe2s.parser.Json._
import Parser._


/**
 * @author Oleksiy_Dyagilev
 */
trait JsonParser extends CharParser {

  def obj = '{' ~> repeatSeparated(entry, ',') <~ '}'  >>  {attrs => JsonObject(attrs)}



  def entry =  entryKey ~ (':' ~> entryVal) >> {case(k,v) => JsonEntry(k, v)}

  def entryKey = '"' ~> charNot('"').* <~ '"'   >>  {v => JsonKey(v.mkString)}

  def entryVal: Parser[JsonVal] = intVal | stringVal | obj | arrayVal | nullVal



  def intVal = intNumber  >>  {i => JsonIntVal(i)}

  def stringVal = '"' ~> charNot('"').* <~ '"'  >> {v => JsonStringVal(v.mkString)}

  def arrayVal = '[' ~> repeatSeparated(entryVal, ',') <~ ']'  >>  {items =>  JsonArray(items)}

  def nullVal = "null" >> {_ => JsonNull()}



  def parseJson(s:String):Either[String, JsonObject] = obj(s) match {
    case Success(res, "") => Right(res)
    case Success(res, rest) => Left(s"cannot parse. rest = $rest")
    case Failure(msg) => Left(msg)
  }

}
