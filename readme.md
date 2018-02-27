# Escalima

Escalima is a Scala wrapper for [esprima](http://esprima.org/) to parse JavaScript into an ast. It works within the JVM as well as with ScalaJS. The ast returned by esprima can either be received as a json string or as a neatly wrapped structure of scala classes.

## Documantation

[The scaladoc for escalima can be found here](https://srtobi.github.io/escalima/docs/escalima/).

## Example

```scala
val parser = new ECMAScript
val source = "test(id)"
val program = parser.parseModule(source)

program match {
    case ast.Program(Seq(stmt)) =>
        stmt match {
            case ast.ExpressionStatement(ast.CallExpression(_, Seq(arg))) =>
                println(arg.toJSON)
        }
}
```

[All classes of the ast](https://srtobi.github.io/escalima/docs/escalima/ast/) have neatly defined apply and unapply methods. The output of above program is:

```json
{
    "type": "Identifier",
    "name": "id",
    "loc": {
        "source": null,
        "start": {
            "line": 1,
            "column": 5
        },
        "end": {
            "line": 1,
            "column": 7
        }
    }
}
```

Instead of an ast Escalima can also just return a json string which is equivalent to the json returned by esprima.
`println(parser.parseModuleToJson(source))` would print:

```json
{
    "body": [
        {
            "type": "ExpressionStatement",
            "expression": {
                "type": "CallExpression",
                "callee": {
                    "type": "Identifier",
                    "name": "test",
                    "loc": {
                        ...
                    }
                },
                "arguments": [
                    {
                        "type": "Identifier",
                        "name": "id",
                        "loc": {
                            ...
                        }
                    }
                ],
                "loc": {
                    ...
                }
            },
            "loc": {
                ...
            }
        }
    ],
    "loc": {
        ...
    }
}
```


## How it works

For **ScalaJS** Escalima simply loads Esprima as a module (that's why it has to be used with some kind of bundler) and wraps the calls.

For the **JVM** it creates a new instance of `javax.script.ScriptManager` and loads Esprima. Then it calls the appropriate functions (See [JVMESPrimaBridge.scala](/escalima/jvm/src/main/scala/escalima/JVMESPrimaBridge.scala)). The initialization of the ScriptManager and Esprima might take a second. It is therefor recommended to only create one instance of [`ECMAScript`](https://srtobi.github.io/escalima/docs/escalima/ECMAScript.html) per thread.
