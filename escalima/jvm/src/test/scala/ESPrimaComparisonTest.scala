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

    private def checkFolder(folder: String, module: Boolean = false): Unit = {
        val dir = new File("esprima/test/fixtures/" + folder)
        checkDirectory(dir, module)
    }

    private def checkDirectory(dir: File, module: Boolean): Unit = {
        assert(dir.exists)
        assert(dir.isDirectory)
        dir.listFiles().foreach(file => {
            if (file.isDirectory) {
                checkDirectory(file, module)
            } else if (file.toString.endsWith(".js")) {
                val parse = if (module) emcaScript.parseModuleToJson _ else emcaScript.parseScriptToJson _
                val checkFile = new File(file.getAbsolutePath.substring(0, file.getAbsolutePath.length - 3) + ".tree.json")
                lazy val checkJson = upickle.json.read(scala.io.Source.fromFile(checkFile).mkString)
                if (checkFile.exists() && !checkJson.obj.contains("errors")) {
                    val source = scala.io.Source.fromFile(file).mkString

                    try {
                        val orgJson = upickle.json.read(parse(source.mkString))
            val newJson = escalima.ast.Program.from(orgJson).toJSON
                        check(orgJson, newJson)
                    } catch {
                        case ESPrimaParseException(msg) =>
                            fail(s"Failed to parse $file!\n $msg)")
                        case e =>
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
    }
}
