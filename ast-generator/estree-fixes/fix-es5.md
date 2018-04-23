# Fixes for es5

This specification fixes all constructs which the scala exporter can not process


## Literal

Literal should be parsed into multiple subclasses. For each primitive type one.
The `Literal.from` function must be explicitly fixed

```js
extend interface Literal {
    value: null
    raw: string
}
```

Add the different literal types:

```js
interface BooleanLiteral <: Literal {
    type: "Literal"
    value: boolean
}
```

```js
interface NumberLiteral <: Literal {
    type: "Literal"
}
```

```js
interface StringLiteral <: Literal {
    type: "Literal"
    value: string
}
```

```js
interface NullLiteral <: Literal {
    type: "Literal"
}
```

```js
interface RegExpLiteral <: Literal {
  type: "Literal"
  regex: {
    pattern: string;
    flags: string;
  };
}
```

```js

```

## Directive

The `directive` is also a `statement`.

```js
interface Directive <: Statement {
    type: "ExpressionStatement";
    expression: Literal;
    directive: string;
}
```

Fix all properties that use a union between `Statement` and `Directive`

```js
extend interface Program {
    body: [ Statement ]
}
```

Also make FunctionBody a simple Statement so there is no conflict with the body of 

```js
interface FunctionBody {
    type: "BlockStatement"
    body: [ Statement ]
}
```

## Property Kind

Move `kind` to an enum

```js
extend interface Property {
    kind: PropertyKind
}

enum PropertyKind {
    "init" | "get" | "set"
}
```

## Property key

Resolve the union type between `Literal`, `Identifier` and `null` in `key`.

```js
// this is not needed because it is overwritten by es2015

/*interface PropertyKey <: Node {}

extend interface Literal <: PropertyKey {}
extend interface Identifier <: PropertyKey {}

extend interface Property {
    key: PropertyKey
}*/
```

## ForStatement Init

Resolve the union type between `VariableDeclaration`, `Expression` and `null` in `init`.

```js

interface ForInit <: Node {}

extend interface VariableDeclaration <: ForInit {}
extend interface Expression <: ForInit {}

extend interface ForStatement {
    init: ForInit | null
}
```

## ForInStatement Left

Resolve the union type between `VariableDeclaration` and `Pattern` in `left`.

```js

interface ForInTarget <: Node {}

extend interface VariableDeclaration <: ForInTarget {}
extend interface Pattern <: ForInTarget {}

extend interface ForInStatement {
    left: ForInTarget
}

```


## AssignmentExpression left

Resolve the union type between `Expression` and `Pattern` in `left`.

```js

interface AssignmentTarget <: Node {}

extend interface Expression <: AssignmentTarget {}
extend interface Pattern <: AssignmentTarget {}

extend interface AssignmentExpression {
    left: AssignmentTarget
}

```


## Conditional Expression property order

The es5 spec first lists `alternate` and then `consequent` for `ConditionalExpression.
That is not the natural order and especially different from `IfStatement`. So turn that around.

```js 
interface ConditionalExpression <: Expression {
    type: "ConditionalExpression";
    test: Expression;
    consequent: Expression;
    alternate: Expression;
}
```