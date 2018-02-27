# Fixes for es2018

```js
interface SpreadableProperty {}

extend interface ObjectExpression {
    properties: [ SpreadableProperty ]
}

extend interface Property <: SpreadableProperty {}
extend interface SpreadElement <: SpreadableProperty {}
```

```js
interface PropertyPattern {}

extend interface ObjectPattern {
    properties: [ PropertyPattern ]
}

extend interface AssignmentProperty <: PropertyPattern {}
extend interface RestElement <: PropertyPattern {}
```