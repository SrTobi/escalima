import java.io.File

import escalima.ECMAScript
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
                        check(value, o.obj(name))
                    }
            }
        case _ => fail(s"$org !== $newValue")
    }

    private def checkFolder(folder: String): Unit = {
        val dir = new File("esprima/test/fixtures/" + folder)
        assert(dir.exists)
        assert(dir.isDirectory)
        dir.listFiles(_.getAbsolutePath.endsWith(".js")).toSeq.foreach(file => {
            val source = scala.io.Source.fromFile(file)

            val orgJson = upickle.json.read(emcaScript.parseScriptToJson(source.mkString))
            val newJson = escalima.ast.Program.from(orgJson).toJSON
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
