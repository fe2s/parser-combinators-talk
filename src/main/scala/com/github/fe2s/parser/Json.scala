package com.github.fe2s.parser

/**
 * @author Oleksiy_Dyagilev
 */
object Json {

  sealed trait JsonVal
  case class JsonStringVal(s:String) extends JsonVal
  case class JsonIntVal(i:Int) extends JsonVal
  case class JsonArray(items: List[JsonVal]) extends JsonVal
  case class JsonNull() extends JsonVal

  case class JsonKey(k:String)

  case class JsonEntry(k:JsonKey, v:JsonVal)

  case class JsonObject(attrs:List[JsonEntry]) extends JsonVal

}
