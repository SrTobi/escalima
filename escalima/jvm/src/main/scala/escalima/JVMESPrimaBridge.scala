package escalima

import javax.script.{Invocable, ScriptEngineManager}

private class JVMESPrimaBridge extends ESPrimaBridge {

    private val engine = new ScriptEngineManager().getEngineByName("js")
    private val esprimaObject = {
        val esprimaCode = io.Source.fromResource("esprima.js").reader()
        engine.eval(esprimaCode)
        engine.get("esprima")
    }
    private val optionObj = engine.eval("var _options = { loc: true }; _options;")

    override def parse(source: String, module: Boolean): String = {
        val method = if (module) "parseModule" else "parseScript"
        val res = engine.asInstanceOf[Invocable].invokeMethod(esprimaObject, method, source, optionObj)
        val json = engine.asInstanceOf[Invocable].invokeMethod(engine.get("JSON"), "stringify", res)
        json.asInstanceOf[String]
    }
}
