import java.io.File

import escalima.{ECMAScript, ESPrimaParseException}
import org.scalatest.{FlatSpec, FreeSpec, Matchers}
import upickle.Js

class ESPrimaComparisonTest extends FreeSpec with Matchers {

    private val emcaScript = new ECMAScript

    private def check(org: Js.Value, newValue: Js.Value): Unit = (org, newValue) match {
        case (Js.Null, Js.Null) =>
        case (Js.True, Js.True) =>
        case (Js.False, Js.False) =>
        case (Js.Str(o), Js.Str(n)) if o == n =>
        case (Js.Arr(o@_*), Js.Arr(n@_*)) => o.zipAll(n, null, null).foreach{ case (or, ne) => check(or, ne) }
        case (_: Js.Num, _) =>
        case (o: Js.Obj, n: Js.Obj) =>
            n.obj.foreach {
                case (name, value) =>
                    if (value != Js.Null) {
                        check(o.obj(name), value)
                    }
            }
        case _ => fail(s"$org !== $newValue")
    }

    private def checkFolder(folder: String): Unit = {
        val dir = new File("esprima/test/fixtures/" + folder)
        checkDirectory(dir)
    }

    private def checkDirectory(dir: File): Unit = {
        assert(dir.exists)
        assert(dir.isDirectory)
        dir.listFiles().foreach(file => {
            if (file.isDirectory) {
                checkDirectory(file)
            } else if (file.toString.endsWith(".js")) {
                val checkFile = new File(file.getAbsolutePath.substring(0, file.getAbsolutePath.length - 3) + ".tree.json")
                lazy val checkJson = upickle.json.read(scala.io.Source.fromFile(checkFile).mkString).obj
                if (checkFile.exists() && !checkJson.contains("errors")) {
                    val source = scala.io.Source.fromFile(file).mkString

                    val module = checkJson("sourceType").str == "module"
                    val parse = if (module) emcaScript.parseModuleToJson _ else emcaScript.parseScriptToJson _
                    try {
                        val orgJson = upickle.json.read(parse(source.mkString))
                        val newJson = escalima.ast.Program.from(orgJson).toJSON
                        check(orgJson, newJson)
                    } catch {
                        case ESPrimaParseException(msg) =>
                            fail(s"Failed to parse $file!\n $msg)")
                        case e: Exception =>
                            info("Failed in " + file)
                            throw e
                    }
                } else {
                    if (!file.toString.contains("invalid"))
                        info("Skip " + file)
                }
            }
        })
    }

    "The ESCMSript class should be able process the test files in" - {
        "whitespace" in {
            checkFolder("whitespace")
        }

        "comment" in {
            checkFolder("comment")
        }

        "automatic-semicolon-insertion" in {
            checkFolder("automatic-semicolon-insertion")
        }

        "expression" in {
            checkFolder("expression")
        }

        "statement" in {
            checkFolder("statement")
        }

        "directive-prolog" in {
            checkFolder("directive-prolog")
        }

        "declaration" in {
            checkFolder("declaration")
        }

        "ES6/arrow-function" in {
            checkFolder("ES6/arrow-function")
        }

        "ES6/binary-integer-literal" in {
            checkFolder("ES6/binary-integer-literal")
        }

        "ES6/binding-pattern" in {
            checkFolder("ES6/binding-pattern")
        }

        "ES6/class" in {
            checkFolder("ES6/class")
        }

        "ES6/default-parameter-value" in {
            checkFolder("ES6/default-parameter-value")
        }

        "ES6/destructuring-assignment" in {
            checkFolder("ES6/destructuring-assignment")
        }

        "ES6/export-declaration" in {
            checkFolder("ES6/export-declaration")
        }

        "ES6/for-of" in {
            checkFolder("ES6/for-of")
        }

        "ES6/generator" in {
            checkFolder("ES6/generator")
        }

        "ES6/identifier" in {
            checkFolder("ES6/identifier")
        }

        "ES6/import-declaration" in {
            checkFolder("ES6/import-declaration")
        }

        "ES6/lexical-declaration" in {
            checkFolder("ES6/lexical-declaration")
        }

        "ES6/meta-property" in {
            checkFolder("ES6/meta-property")
        }

        "ES6/method-definition" in {
            checkFolder("ES6/method-definition")
        }

        "ES6/object-initialiser" in {
            checkFolder("ES6/object-initialiser")
        }

        "ES6/object-literal-property-value-shorthand" in {
            checkFolder("ES6/object-literal-property-value-shorthand")
        }

        "ES6/octal-integer-literal" in {
            checkFolder("ES6/octal-integer-literal")
        }

        "ES6/rest-parameter" in {
            checkFolder("ES6/rest-parameter")
        }

        "ES6/spread-element" in {
            checkFolder("ES6/spread-element")
        }

        "ES6/super-property" in {
            checkFolder("ES6/super-property")
        }

        "ES6/template-literals" in {
            checkFolder("ES6/template-literals")
        }

        "ES6/unicode-code-point-escape-sequence" in {
            checkFolder("ES6/unicode-code-point-escape-sequence")
        }

        "ES6/yield" in {
            checkFolder("ES6/yield")
        }

        "ES2016" in {
            checkFolder("ES2016")
        }

        "es2017" in {
            checkFolder("es2017")
        }

        "es2018" in {
            checkFolder("es2018/rest-property")
            checkFolder("es2018/spread-property")
        }
    }
}
