package escalima

/**
  * The main class of Escalima.
  *
  * Has methods to parse JavaScript.
  *
  * @note Creation of this class might be expensive! It is recommended to only create one per thread.
  */
class ECMAScript {
    private val bridge: ESPrimaBridge = ESPrimaBridgeCreator.create()

    /**
      * Parses a JavaScript module and returns it's ast.
      *
      * @note JavaScript modules and JavaScript scripts are different things. Scripts for example may not contain import/export declarations.
      *
      * @param sourcecode the source code that should be parsed
      * @return the ast of the module
      *
      * @throws ESPrimaParseException if esprima was not able to parse sourcecode
      */
    def parseModule(sourcecode: String): ast.Program = convertToAst(parseModuleToJson(sourcecode))

    /**
      * Parses a JavaScript script and returns it's ast.
      *
      * @note JavaScript modules and JavaScript scripts are different things. Scripts for example may not contain import/export declarations.
      *
      * @param sourcecode the source code that should be parsed
      * @return the ast of the module
      *
      * @throws ESPrimaParseException if esprima was not able to parse sourcecode
      */
    def parseScript(sourcecode: String): ast.Program = convertToAst(parseScriptToJson(sourcecode))

    /**
      * Parses a JavaScript module and returns the json created by esprima.
      *
      * @note JavaScript modules and JavaScript scripts are different things. Scripts for example may not contain import/export declarations.
      *
      * @param sourcecode the source code that should be parsed
      * @return the json of the ast created by esprima
      *
      * @throws ESPrimaParseException if esprima was not able to parse sourcecode
      */
    def parseModuleToJson(sourcecode: String): String = bridge.parse(sourcecode, module = true)

    /**
      * Parses a JavaScript script and returns the json created by esprima.
      *
      * @note JavaScript modules and JavaScript scripts are different things. Scripts for example may not contain import/export declarations.
      *
      * @param sourcecode the source code that should be parsed
      * @return the json of the ast created by esprima
      *
      * @throws ESPrimaParseException if esprima was not able to parse sourcecode
      */
    def parseScriptToJson(sourcecode: String): String = bridge.parse(sourcecode, module = false)

    private def convertToAst(json: String): ast.Program = ast.Program.from(upickle.json.read(json))
}
