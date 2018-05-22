# Fixes for es2015

This specification fixes all constructs which the scala exporter can not process


## Program

A Program can now be a script or a module

```js
interface ModuleStatement <: Node {}

interface Program <: Node  {
    body: [ ModuleStatement ]
}

extend interface Statement <: ModuleStatement {}
extend interface ModuleDeclaration <: ModuleStatement {}
```


## Varibale declaration kind

```js
extend interface VariableDeclaration {
    kind: VariableDeclarationKind
}

enum VariableDeclarationKind {
    "var" | "let" | "const"    
}
```


## Spread Element

In some context it is possible to use a spread element.

```js
interface SpreadableExpression <: Node  {}

extend interface Expression <: SpreadableExpression {}
extend interface SpreadElement <: SpreadableExpression {}
```

Fix the possible contexts:

```js
extend interface ArrayExpression {
    elements: [ SpreadableExpression | null ];
}

extend interface CallExpression {
    arguments: [ SpreadableExpression ];
}

extend interface NewExpression {
    arguments: [ SpreadableExpression ];
}
```

## Callee could be super

```js
interface Callee <: Node {}

extend interface Expression <: Callee {}
extend interface Super <: Callee {}

extend interface CallExpression {
    callee: Callee;
}

extend interface MemberExpression {
    object: Callee;
}

```

## ArrowFunctionExpression

Fix the union type in `ArrowFunctionExpression`. `ArrowFunctionExpression` inherited originally from `Function`. This breaks the body type in `Function`. So remove this dependency.

```js
interface ArrowFunctionBody <: Node {}

extend interface FunctionBody <: ArrowFunctionBody {}
extend interface Expression <: ArrowFunctionBody {}

interface ArrowFunctionExpression <: Expression {
    type: "ArrowFunctionExpression"
    id: Identifier | null;
    params: [ Pattern ];
    body: ArrowFunctionBody
    async: boolean
}
```

## ObjectPattern

Do not let AssignmentProperty inherit Property

```js
interface AssignmentProperty <: Node {
    type: "Property"; // inherited
    value: Pattern;
    kind: "init";
    method: false;
}
```

## MethodDefinition::kind

```js
extend interface MethodDefinition {
    kind: MethodKind
}

enum MethodKind {
    "constructor" | "method" | "get" | "set"
}
```


## ImportDeclaration::specifiers

```js
extend interface ImportDeclaration {
    specifiers: [ ModuleImportSpecifier ]
}

interface ModuleImportSpecifier <: ModuleSpecifier {}

interface ImportSpecifier <: ModuleImportSpecifier {
    type: "ImportSpecifier";
    imported: Identifier;
}

interface ImportDefaultSpecifier <: ModuleImportSpecifier {
    type: "ImportDefaultSpecifier";
}

interface ImportNamespaceSpecifier <: ModuleImportSpecifier {
    type: "ImportNamespaceSpecifier";
}
```

## ExportDefaultDeclaration::declaration

```js
extend interface ExportDefaultDeclaration {
    declaration: Exportable;
}

interface Exportable <: Node {}

extend interface Declaration <: Exportable {}
extend interface Expression <: Exportable {}
```

## null id in FunctionDeclaration and ClassDeclaration

`FunctionDeclaration`'s id is not supposed to be null (acording to the es5.md specification).
Unfortunately it is in a default export declaration.

```js
interface FunctionDeclaration <: Function, Declaration {
    type: "FunctionDeclaration";
}
```

Same goes for ClassDeclaration

```js
interface ClassDeclaration <: Class, Declaration {
    type: "ClassDeclaration";
}
```

##