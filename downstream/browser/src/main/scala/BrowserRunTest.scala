import escalima.ECMAScript

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("BrowserRunTest")
object BrowserRunTest {
    @JSExport
    def parse(code: String): String = {
        val ecma = new ECMAScript
        ecma.parseScriptToJson(code)
    }
}
