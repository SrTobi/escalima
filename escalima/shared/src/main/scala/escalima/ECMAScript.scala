package escalima

class ECMAScript {
    private val bridge: ESPrimaBridge = ESPrimaBridgeCreator.create()

    def parseModule(sourcecode: String): ast.Program = convertToAst(parseModuleToJson(sourcecode))
    def parseScript(sourcecode: String): ast.Program = convertToAst(parseScriptToJson(sourcecode))

    def parseModuleToJson(sourcecode: String): String = bridge.parse(sourcecode, module = true)
    def parseScriptToJson(sourcecode: String): String = bridge.parse(sourcecode, module = false)

    private def convertToAst(json: String): ast.Program = ast.Program.from(upickle.json.read(json))
}
