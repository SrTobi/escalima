package escalima

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSImport, ScalaJSDefined}

@ScalaJSDefined
class ESPrimaOptions(val loc: Boolean) extends js.Object

@js.native
@JSImport("./esprima.js", JSImport.Namespace)
private object ESPrima extends js.Object{
    def parseModule(source: String, options: ESPrimaOptions): js.Object = js.native
    def parseScript(source: String, options: ESPrimaOptions): js.Object = js.native
}
