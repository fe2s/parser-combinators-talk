package com.github.fe2s.parser

/**
 * @author Oleksiy_Dyagielv
 */
object Main extends App {


  new CharParser {
//    runParser("1abc", digit)
//    runParser("1abc", digit)
//    runParser("aaa", zeroToMany(char))
//    runParser("aaa", zeroToMany(digit))
//    runParser("123aaa1", zeroToMany(digit))
//    runParser("{asdada}", char('{'))
//    runParser("{asdada}", takeFirst(char('{'), zeroToMany(char)))
//    runParser("{asdada}", takeSecond(char('{'), zeroToMany(char)))
//
//    runParser("{asdada}",
//      takeSecond(char('{'), takeFirst(zeroToMany(charNot('}')), char('}')))
//    )
//
//    // shortcuts
//
//    runParser("{asdada}", char('{') ~> char.*)
//
//    runParser("{asdada}",
//      char('{') ~> charNot('}').* <~ char('}')
//    )
//
  }

    new JsonParser {

//      runParser(""" "name":"John" """.trim,
//        entry
//      )
//      runParser(""" {"name":"John"} """.trim,
//        obj
//      )
//      runParser(""" {"name":"John","lastname":"Doe"} """.trim, obj)
//
//      runParser(""" {} """.trim, obj)

      val json = parseJson(""" {"name":"John","lastname":"Doe","age":55,"hobbies":["tennis","football"],"pet":null} """.trim)
      println(json)

    }


}
