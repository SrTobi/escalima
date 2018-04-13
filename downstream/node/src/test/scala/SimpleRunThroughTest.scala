import escalima.{ECMAScript, ast}
import org.scalatest.{FlatSpec, Inside, Matchers}

class SimpleRunThroughTest extends FlatSpec with Matchers with Inside {

    private val emcaScript = new ECMAScript

    "Escalima" should "work in Node" in {
        val program = emcaScript.parseScript("call(5)")

        inside(program) {
            case ast.Program(Seq(callStmt)) =>
                inside(callStmt) {
                    case ast.ExpressionStatement(expr) =>
                        inside(expr) {
                            case ast.CallExpression(callee, Seq(arg)) =>
                                inside(callee) {
                                    case ast.Identifier(name) =>
                                        name shouldBe "call"
                                }

                                inside(arg) {
                                    case ast.Literal(raw) =>
                                        raw shouldBe "5"
                                }
                        }
                }
        }
    }
}