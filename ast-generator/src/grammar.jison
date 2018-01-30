/*
 *  This file is a modified version of https://github.com/estree/formal/blob/b52e67b11dfd4e65add84db918576aea27487d65/src/grammar.jison
 */

%{
    var extendInterface = require('./spec').extendInterface;

    function toObject(array, resolveConflicts) {
        return array.reduce(function (obj, item) {
            var oldValue = obj[item.name];
            var newValue = item.value;
            if (oldValue && resolveConflicts) {
                newValue = resolveConflicts(oldValue, newValue);
            }
            obj[item.name] = newValue;
            return obj;
        }, Object.create(null));
    }

    function toProperties(array) {
        var checkSet = {}
        return array.map(function (prop) {
            var name = prop.name
            if (checkSet[name]) {
                throw new Error("Property '" + name + "' is defined twice!")
            }
            checkSet[name] = true
            return [name, prop.value]
        })
    }

    function intf(name, props, base, extend) {
        return {
            name: name,
            value: {
                extend: extend,
                kind: 'interface',
                props: props,
                base: base
            }
        };
    }
%}

/* lexical grammar */
%lex
%%
'//'.*                /* skip one-line comments */
'/*'[\s\S]*?'*/'      /* skip multi-line comments */
\s+                   /* skip whitespace */
'interface'           return 'INTERFACE'
'enum'                return 'ENUM'
'extend'              return 'EXTEND'
'"'.*?'"'             return 'STRING';
\d+                   return 'NUMBER';
'true'|'false'        return 'BOOLEAN';
'null'                return 'NULL';
[A-Za-z_][\w]*        return 'NAME';
[,;:[\]{}|]|'<:'      return yytext
<<EOF>>               return 'EOF'

/lex

/* operator associations and precedence */

%left ';'
%left ',' '|'

%start program

%ebnf

%% /* language grammar */

program
    : def* EOF {
        return toObject($$, function (oldValue, newValue) {
            // Extend earlier found interface or return new one.
            if (oldValue.kind === 'interface' && newValue.kind === 'interface' && newValue.extend) {
                return extendInterface(oldValue, newValue)
            } else {
                return newValue;
            }
        });
    }
    ;

def
    : EXTEND INTERFACE NAME object -> intf($NAME, $object, [], true)
    | EXTEND INTERFACE NAME '<:' names object -> intf($NAME, $object, $names, true)
    | INTERFACE NAME '<:' names object -> intf($NAME, $object, $names, false)
    | INTERFACE NAME object -> intf($NAME, $object, [], false)
    | ENUM NAME '{' enumBody '}' -> {name: $NAME, value: {kind: 'enum', values: $enumBody}}
    ;

names
    : names ',' NAME {
        $$.push($NAME);
    }
    | NAME -> [$NAME]
    ;

enumBody
    : enumBody '|' literal {
        $$.push($literal);
    }
    | literal -> [$literal]
    ;

object
    : '{' prop* '}' -> toProperties($2)
    ;

prop
    : NAME ':' $type ';'? -> {name: $NAME, value: $type}
    ;

type
    : type '|' type {
      if ($$.kind !== 'union') $$ = {kind: 'union', types: [$$]};
      $$.types.push($type2);
    }
    | literal -> {kind: 'literal', value: $literal}
    | NAME -> {kind: 'reference', name: $NAME}
    | '[' type ']' -> {kind: 'array', base: $type}
    | object -> {kind: 'object', items: $object}
    ;

literal
    : STRING -> $STRING.slice(1, -1)
    | NUMBER -> Number($NUMBER)
    | BOOLEAN -> $BOOLEAN === 'true'
    | NULL -> null
    ;