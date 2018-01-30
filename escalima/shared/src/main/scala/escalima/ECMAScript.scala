package escalima

class ECMAScript {
    private val bridge: ESPrimaBridge = ESPrimaBridgeCreator.create()

    def parseModule(sourcecode: String): ast.Program = convertToAst(bridge.parse(sourcecode, module = true))
    def parseScript(sourcecode: String): ast.Program = convertToAst(bridge.parse(sourcecode, module = false))

    private def convertToAst(json: String): ast.Program = ast.Program.from(upickle.json.read(json))
}
