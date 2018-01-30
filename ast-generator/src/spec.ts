export type Properties = [string, Entry][]

export interface Interface {
    extend: boolean
    kind: "interface"
    props: Properties
    base: string[]
}

export interface Reference {
    kind: "reference"
    name: string
}

type LiteralValue = string | boolean | number | null

export interface Literal {
    kind: "literal"
    value: LiteralValue
}

export interface Union {
    kind: "union"
    types: Entry[]
}

export interface Array {
    kind: "array"
    base: Entry
}

export interface Enum {
    kind: "enum"
    values: LiteralValue[]
}

export interface Object {
    kind: "object"
    items: Properties
}

export type Definition = Interface | Enum
export type Entry = Reference | Literal | Union | Array | Object

export interface Specification {
    [name: string]: Definition
}

export function extendInterface(target: Interface, extend: Interface): Interface {
    if (!extend.extend) {
        throw new Error("Called extendInterface with interface that should replace original")
    }
    function onlyUnique(value: string, index: number, self: string[]) { 
        return self.indexOf(value) === index;
    }
    
    const additionalProps = extend.props.filter(([name, entry]) => {
        const prop: [string, Entry | null] | undefined = target.props.find(([myname,]) => myname === name)
        if (prop) {
            prop[1] = (entry.kind === "literal" && entry.value === null)? null : entry
            return false
        } else {
            return true
        }
    })

    return {
        extend: target.extend,
        kind: "interface",
        props: target.props.concat(additionalProps).filter(b => b[1] /* filter out all removed values*/),
        base: target.base.concat(extend.base).filter(onlyUnique)
    }
}