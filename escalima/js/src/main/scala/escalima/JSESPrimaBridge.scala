package escalima

import scala.scalajs.js.JSON

private class JSESPrimaBridge extends ESPrimaBridge {
    override def parse(source: String, module: Boolean): String = {
        val parse = if (module) ESPrima.parseModule _ else ESPrima.parseScript _
        val options = new ESPrimaOptions(true)
        val astJson = parse(source, options)
        JSON.stringify(astJson)
    }
}
