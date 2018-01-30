package escalimapredef

import escalima.ECMAScript
import upickle.Js

object Playground {
    def main(args: Array[String]): Unit = {
        val parser = new ECMAScript

        val test = parser.parseModule("if (test) {}")
        println(test.toJSON.toString())
    }
}
