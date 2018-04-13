package escalima.ast

import org.scalatest.{FlatSpec, Matchers}

class AstTest extends FlatSpec with Matchers {

    "ast nodes" should "be castable to Node" in {
        {
            val statement: Statement = null
            val node: Node = statement
            node shouldBe null
        }
        {
            val expression: Expression = null
            val node: Node = expression
            node shouldBe null
        }
    }

}
