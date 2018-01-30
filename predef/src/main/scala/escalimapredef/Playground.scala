package escalimapredef

import escalima.ECMAScript

object Playground {
    def main(args: Array[String]): Unit = {
        val parser = new ECMAScript
        val test = parser.parseModule("test")
        println(test)
    }
}
