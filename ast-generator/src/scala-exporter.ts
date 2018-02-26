import * as Spec from './spec'
import Specification = Spec.Specification
import lazy from './lazy';
import nonrec from './nonrec'
import once from './once'
import { Interface } from './spec';
import indentString = require('indent-string')
import * as assert from 'assert'
import { prototype } from 'events';

export interface Options {
    package: string
    discriminant: string
    indentionString?: string

    forceClasses: string[]
    forceAbstract: string[]
    forceTrait: string[]
    suppressFromJson: string[]
    renames: { from: string, to: string}[]
    parseConditions: { [target: string]: (src: string) => string }
}

export interface NativeType {
    name: string
    native: string

    fromExpr(src: string): string
}

function beginUpper(str: string): string {
    return str.charAt(0).toUpperCase() + str.slice(1);
}

function beginLower(str: string): string {
    return str.charAt(0).toLowerCase() + str.slice(1);
}

function unique<T>(value: T, index: number, self: T[]): boolean { 
    return self.indexOf(value) === index;
}

function makeActual(str: string): string {
    return "actual" + beginUpper(str)
}
export function buildScalaAst(spec: Specification, options: Options): string {

    const state = {
        refsResolvable: false,
        basesMarked: false
    }

    function norm(str: string) {
        const rename = options.renames.find(({from}) => from == str)
        return rename? rename.to : str
    }

    const definitions = new Map<string, Definition | NativeEntry>()
    
    function resolve(name: string): Definition | NativeEntry {
        assert(state.refsResolvable)
        const resolved = definitions.get(name)

        if (!resolved) {
            throw new Error("Couldn't resolve " + name)
        }
        
        return resolved
    }

    const indentionString = options.indentionString || "\t"
    class Writer {
        private buffer: string = ""
        private dependentWriter = 0
        private ended = false

        constructor(private indention: number = 0,
                    private parent: Writer | undefined = undefined) {

            if (parent && parent.ended) {
                throw new Error("can not create dependent writer on an ended writer")
            }
            
            if (parent) {
                parent.dependentWriter++
            }
        }

        println(txt?: string): void {
            if (this.ended) {
                throw new Error("can write on an ended writer")
            }

            let line = (txt || "") + "\n"
            if (this.indention > 0) {
                line = indentString(line, this.indention, indentionString)
            }
            this.buffer += line
        }

        indented(offset = 1): Writer {
            return new Writer(this.indention + offset, this)
        }

        sub(): Writer {
            return this.indented(0)
        }

        end(): void {
            if (this.dependentWriter > 0) {
                throw new Error("Can not end writer before all dependent writers have ended")
            }

            if (this.ended) {
                throw new Error("Can not end writer twice")
            }

            if (this.parent) {
                this.parent.buffer += this.buffer
                this.parent.dependentWriter--
            }
            this.ended = true
        }

        toString(): string {
            return this.buffer
        }
    }

    function innerToEntry(entry: Spec.Entry, outer: NamedEntry, property?: string): Entry {
        switch (entry.kind) {
            case "reference": return resolve(entry.name)
            case "array": return new ArrayEntry(entry, outer, property)
            case "literal": return new LiteralEntry(entry, outer, property)
            case "union": return new UnionEntry(entry, outer, property)
            case "object": return new ObjectEntry(entry, outer, property)
        }
    }

    function definitionToEntry(entry: Spec.Definition, name: string): Definition {
        switch (entry.kind) {
            case "enum": return new EnumEntry(name, entry)
            case "interface": return new InterfaceEntry(name, entry, false)
        }
    }

    abstract class Entry {
        readonly org: Spec.Entry | Spec.Definition | undefined

        abstract inners: Entry[]

        abstract asScalaType(): string
        abstract fromExpr(src: string, optionSrc: string | undefined): string
        abstract toExpr(self: string): string

        canBeParam(): boolean {
            return true
        }

        isSubtypeOf(other: Entry): boolean {
            if (this === other) {
                return true
            }

            if (this._isSubtypeOf(other)) {
                return true
            }

            if (other instanceof UnionEntry) {
                return this.isSubtypeOf(other.type)
            }
            return false
        }

        convertFrom(from: Entry, src: string): string {
            assert(from.isSubtypeOf(this))
            return src
        }

        protected abstract _isSubtypeOf(other: Entry): boolean
    }

    interface NamedEntry {
        readonly name: string
    }

    abstract class InnerEntry extends Entry {
        abstract readonly org: Spec.Entry
        abstract readonly outer: NamedEntry
        abstract readonly property: string | undefined

        protected toEntry(entry: Spec.Entry): Entry {
            return innerToEntry(entry, this.outer, this.property)
        }
    }

    class ArrayEntry extends InnerEntry {
        constructor(public readonly org: Spec.Array,
                    public readonly outer: NamedEntry,
                    public readonly property: string | undefined) {
            super()
        }

        @lazy
        get base(): Entry {
            return this.toEntry(this.org.base)
        }

        @lazy
        get inners(): Entry[] {
            return [this.base]
        }

        asScalaType(): string {
            return `Seq[${this.base.asScalaType()}]`
        }

        fromExpr(src: string, optionSrc: string): string {
            return `${src}.arr.map(elem => ${this.base.fromExpr("elem", undefined)})`
        }

        toExpr(self: string): string {
            return `Js.Arr(${self}.map(inner => ${this.base.toExpr("inner")}): _*)`
        }

        _isSubtypeOf(other: Entry): boolean {
            if (other instanceof ArrayEntry) {
                return this.base.isSubtypeOf(other.base)
            }
            return false
        }
    }

    class UnionEntry extends InnerEntry {
        constructor(public readonly org: Spec.Union,
                    public readonly outer: NamedEntry,
                    public readonly property: string | undefined) {
            super()
        }

        @lazy
        get types(): Entry[] {
            return this.org.types.map(ty => this.toEntry(ty))
        }

        @lazy
        get inners(): Entry[] {
            return this.types
        }

        @lazy
        get nonNullTypes(): Entry[] {
            return this.types.filter(entry => !(entry instanceof LiteralEntry) || entry.value !== null)
        }

        get isOption(): boolean {
            return this.types.length !== this.nonNullTypes.length
        }

        get type(): Entry {
            if (!this.isOption || this.nonNullTypes.length > 1) {
                throw new Error(`in ${this.outer.name}: union types with more than 1 non null type are not supported`)
            }

            return this.nonNullTypes[0]
        }

        asScalaType(): string {
            return `Option[${this.type.asScalaType()}]`
        }

        fromExpr(src: string, srcOption: string): string {
            if (srcOption) {
                return `${srcOption}.flatMap(_ match { case Js.Null => None; case some => Some(some)}).map(inner => ${this.type.fromExpr("inner", undefined)})`
            } else {
                return `(${src} match { case Js.Null => None; case some => Some(some)}).map(inner => ${this.type.fromExpr("inner", undefined)})`
            }
        }

        toExpr(self: string): string {
            return `${self}.map(inner => ${this.type.toExpr("inner")}).getOrElse(Js.Null)`
        }

        _isSubtypeOf(other: Entry): boolean {
            if (other instanceof UnionEntry) {
                return this.isOption === other.isOption && this.type.isSubtypeOf(other.type)
            }
            return other === this
        }

        convertFrom(from: Entry, src: string): string {
            assert(from.isSubtypeOf(this))
            if (from instanceof UnionEntry) {
                return `${src}.map(inner => ${this.type.convertFrom(from.type, "inner")})`
            } else {
                return `Some(${this.type.convertFrom(from, src)})`
            }
        }
    }
    
    class LiteralEntry extends InnerEntry {
        constructor(public readonly org: Spec.Literal,
                    public readonly outer: NamedEntry,
                    public readonly property: string | undefined) {
            super()
        }

        @lazy
        get value(): string | boolean | null {
            const value =  this.org.value

            if (typeof value === "number") {
                throw new Error("literal type can not be a number")
            }

            return value
        }

        @lazy
        get inners(): Entry[] {
            return []
        }

        asScalaType(): string {
            const value = this.value
            if (typeof value === "object") {
                return "null"
            } else if (typeof value === "string") {
                return `"${value}"`
            } else {
                return value.toString()
            }
        }


        toExpr(self: string): string {
            const value = this.value
            if (typeof value === "object") {
                return "Js.Null"
            } else if (typeof value === "string") {
                return `Js.Str(${value})`
            } else {
                return value? "Js.True" : "Js.False"
            }
        }

        canBeParam(): boolean {
            return false
        }

        fromExpr(src: string): string {
            throw new Error("")
        }

        _isSubtypeOf(other: Entry): boolean {
            if (other instanceof LiteralEntry) {
                return this.value === other.value
            } else if (other instanceof NativeEntry) {
                if (typeof this.value === "string") {
                    return other.name === "string"
                } else if (typeof this.value === "number") {
                    return other.name === "number"
                }
            }
            return false
        }
    }

    type Properties = [string, Entry][]
    const Properties = {
        get: (props: Properties, name: string): Entry | undefined => {
            for (const [myname, entry] of props) {
                if (myname == name) {
                    return entry
                }
            }
            return undefined
        },
        values: (props: Properties): Entry[] => {
            return props.map(([,entry]) => entry)
        }
    }

    function convertProperties(props: Spec.Properties, outer: NamedEntry): Properties {
        const result: Properties = props
            // filter null properties
            .filter(([,entry]) => !(entry.kind === "literal" && entry.value === null))
            .map(([name, entry]): [string, Entry] => {
            return [name, innerToEntry(entry, outer, name)]
            })
        return result
    }

    class ObjectEntry extends InnerEntry implements NamedEntry {
        constructor(public readonly org: Spec.Object,
                    public readonly outer: NamedEntry,
                    public readonly property: string | undefined) {
            super()
        }

        @lazy
        get props(): Properties {
            return convertProperties(this.org.items, this)
        }

        @lazy
        get name(): string {
            if (!this.property) {
                throw new Error(`Can not create name for object!`)
            }
            return beginUpper(this.property)
        }

        @lazy
        get inners(): Entry[] {
            return Properties.values(this.props)
        }

        asScalaType(): string {
            this.printObjectType()
            return norm(this.name)
        }

        @once
        private printObjectType(): void {
            const intf = new InterfaceEntry(this.name, {
                extend: false,
                kind: "interface",
                base: [],
                props: this.org.items
            }, true)

            intf.printDefinition(typeWriter)
        }

        fromExpr(src: string): string {
            return `${this.asScalaType()}.from(${src})`
        }

        toExpr(self: string): string {
            return `${self}.toJSON`
        }

        _isSubtypeOf(other: Entry): boolean {
            if (other instanceof ObjectEntry) {
                return this.name === other.name
            }
            return false
        }
    }

    class NativeEntry extends Entry implements NamedEntry {
        constructor(public readonly name: string,
                    public readonly native: string,
                    private readonly _fromExpr: (src: string) => string,
                    private readonly _toExpr: (src: string) => string) {
            super()
        }

        readonly org: undefined = undefined

        @lazy
        get inners(): Entry[] {indentionString
            return []
        }

        asScalaType(): string {
            return this.native
        }

        fromExpr(src: string): string {
            return this._fromExpr(src)
        }

        toExpr(self: string): string {
            return this._toExpr(self)
        }

        _isSubtypeOf(other: Entry): boolean {
            return other === this
        }
    }

    abstract class Definition extends Entry implements NamedEntry {
        abstract readonly org: Spec.Definition
        
        protected toEntry(entry: Spec.Entry): Entry {
            return innerToEntry(entry, this, undefined)
        }

        readonly name: string

        public inherited = false

        abstract printDefinition(writer: Writer): void
    }

    // [name, entry, owned by current interface, is specialisation]
    type ParamOverride = "normal" | "overrides" | "overridden"
    type Param = [string, Entry, boolean, ParamOverride]

    class InterfaceEntry extends Definition {
        constructor(public readonly name: string,
                    public readonly org: Spec.Interface,
                    private readonly isInnerClass: boolean) {
            super()
        }

        @lazy
        get props(): Properties {
            return convertProperties(this.org.props, this)
        }

        @lazy
        get directBases(): InterfaceEntry[] {
            return this.org.base.map(base => {
                const resolved = resolve(base)
                if (!(resolved instanceof InterfaceEntry)) {
                    throw new Error("Can not inherit native type!")
                }
                return resolved
            })
        }
        
        @lazy
        get bases(): InterfaceEntry[] {
            const directBases = this.directBases

            //if (!directBases.every(base => !base.isTrait)) {
            // we are a class but we only inherit traits
            // check if the traits depend on real classes
            const allClassBases = this.directBases.concat(...this.directBases.map(b => b.allBases))
                                    .filter(b => !b.isTrait)
                                    .filter(unique)
            
            if (allClassBases.length === 0) {
                return directBases
            }
            // search for one class that inherits all other classes
            const mostSpecificClass = allClassBases.find(b => allClassBases.every(bb => b.isSubtypeOf(bb)))
            
            if (!mostSpecificClass) {
                throw new Error(`Multiple inheritance detected for ${this.name} extinding ${allClassBases.map(b => b.name).join(", ")}`)
            }
            return [mostSpecificClass].concat(directBases).filter(unique)
        }

        @lazy
        get allBases(): InterfaceEntry[] {
            return this.bases.concat(...this.bases.map(b => b.allBases)).filter(unique)
        }

        @lazy
        get inners(): Entry[] {
            return Properties.values(this.props).concat(this.bases)
        }

        @lazy
        get base(): InterfaceEntry | undefined {
            assert(state.basesMarked)
            const baseClasses = this.bases.filter(base => !base.isTrait)
            if (baseClasses.length > 1) {
                throw new Error("Can not have more than one base class")
            }

            return baseClasses[0]
        }

        @lazy
        get baseTraits(): InterfaceEntry[] {
            return this.bases.filter(base => base.isTrait)
        }

        private inheritingIntfs: InterfaceEntry[] = []
        get isTrait() {
            assert(state.basesMarked)
            return this.inheritingIntfs.length > 0 && options.forceClasses.indexOf(this.name) === -1
        }

        @lazy
        get isAbstract(): boolean {
            return this.isTrait || options.forceAbstract.indexOf(this.name) >= 0
        }

        addToBaseIntf() {
            this.directBases.forEach(base => {
                base.inheritingIntfs.push(this)
            })
        }

        @lazy
        get needDiscriminant(): boolean {
            return this.bases.findIndex((base) => {
                return Properties.get(this.props, options.discriminant) !== undefined ||  base.needDiscriminant
            }) >= 0
        }

        @lazy
        get allInheritingIntf(): InterfaceEntry[] {
            return this.getAllInheritingIntf().filter(unique)
        }

        @nonrec((): void => { throw new Error("Error! recursive inheritance")})
        private getAllInheritingIntf(): InterfaceEntry[] {
            return this.inheritingIntfs.reduce((acc, intf) => {
                return acc.concat([intf], intf.allInheritingIntf)
            }, [] as InterfaceEntry[])
        }

        @lazy
        private get params(): Param[] {
            const my = this.props
                            .filter(([name,,]) => name !== options.discriminant)
                            .filter(([, entry,]) => entry.canBeParam())
                            .map(([name, entry,]): Param => [name, entry, true, "normal"])
            if (this.base) {
                const baseParams = this.base.params.map(
                    ([name, entry,,]: Param): Param => {
                        const isOverride = Boolean(my.find(([myname, myentry,,]) => {
                            return myname === name && myentry.isSubtypeOf(entry)
                        }))
                        return [name, entry, false, isOverride? "overridden" : "normal"]
                    })
                my.forEach((me) => baseParams.forEach(([thname,thtype,,isOverride]) => {
                    const myPropName = me[0]
                    if (myPropName === thname) {
                        if (!isOverride) {
                            throw new Error(`${this.name} and one of it's base classes both have property '${myPropName}'`)
                        } else {
                            me[3] = "overrides"
                        }
                    }
                }))
                
                return my.concat(baseParams)
            } else {
                return my
            }
        }

        printDefinition(targetWriter: Writer) {
            console.log(`Print definition for ${this.isTrait? "trait" : (this.isAbstract? "abstract class" : "class")}`, this.name)
            if (this.isInnerClass) {
                assert(this.allInheritingIntf.length === 0)
                assert(!this.isTrait)
                assert(this.bases.length === 0)
            }
            function toName([name,,,overmode]: Param) {
                name = norm(name)
                if (overmode === "normal") {
                    return name
                }
                return makeActual(name)
            }
            function toType([, entry,,]: Param) {
                return entry.asScalaType()
            }
            function toTypedName(p: Param) {
                return `${toName(p)}: ${toType(p)}`
            }
            function onlyBaseParams([,, isOwn,]: Param) {
                return !isOwn
            }
            function onlyOwnParams([,, isOwn,]: Param) {
                return isOwn
            }
            function onlyRealParameter([,,,mode]: Param) {
                return mode !== "overridden"
            }
            const name = this.name
            const params = this.params
            const realParams = params.filter(onlyRealParameter)
            const ownParams = params.filter(onlyOwnParams)
            const writer = targetWriter.sub()
            const base = this.base

            const discriminant = options.discriminant
            const discriminantEntry = Properties.get(this.props, discriminant)

            const [firstWith, ...withs] = this.baseTraits.map(base => base.name).map(norm)
            let withExpr = firstWith? ` extends ${firstWith}` : ""
            withExpr += withs.length == 0? "" : (" with " + withs.join(" with "))

            const isAbstract = this.isAbstract
            const hasOwnParams = ownParams.length > 0
            if (this.isTrait) {
                const hasContent = !!base || hasOwnParams

                writer.println(`sealed trait ${norm(name)}${withExpr}${(hasContent)? " {" : ""}`)

                const innerWriter = writer.indented()
                if (base) {
                    innerWriter.println(`this: ${norm(base.name)} =>`)
                    innerWriter.println()
                }
                ownParams.forEach(p => {
                    innerWriter.println(`def ${toTypedName(p)}`)
                })

                if (ownParams.length > 0) {
                    innerWriter.println()
                }
                innerWriter.println(`def toJSON: Js.Value`)
                innerWriter.end()
                writer.println("}")
                writer.println()
            } else {
                if (this.isAbstract && this.inheritingIntfs.length === 0) {
                    throw new Error(`${name} is abstract but never inherited from!`)
                } 
                // check if all traits have been implemented
                this.allBases.filter(b => b.isTrait).forEach(b => {
                    b.props.forEach(([propname, proptype]) => {
                        if (propname !== discriminant &&
                            !ownParams.find(([pname, ptype]) => pname === propname /*&& ptype.isSubtypeOf(proptype)*/)) {
                            
                            console.log(`Base classes of ${norm(name)}`)
                            this.allBases.forEach(b => {
                                console.log(`${b.isTrait? "trait" : "class"} ${norm(b.name)}(${b.params.map(toTypedName).join(", ")})`)
                            })
                            console.log("For")
                            console.log(`${this.isTrait? "trait" : "class"} ${norm(name)}(${ownParams.map(toTypedName).join(", ")})`)
                            console.log()
                            throw new Error(`${norm(name)} did not or did wrongly implemented ${norm(propname)}`)
                        }
                    })
                })

                let argListExpr = realParams.map(p => {
                    const [,, isOwn,] = p
                    return `${isOwn? "val " : ""}${toTypedName(p)}`
                }).join(", ")
                let extendExpr = ""
                if (base) {
                    extendExpr = ` extends ${norm(base.name)}`
                    const baseParams = params.filter(onlyBaseParams)
                    if (baseParams.length > 0) {
                        const extendParamExprs = baseParams.map(([name, baseType,, mode]) => {
                            if (mode == "overridden") {
                                // find actual param and determine conversion
                                const actual = params.find(([actualName,,,]) => name === actualName)
                                if (!actual) {
                                    throw new Error("Did not find the actual param... this should not happen")
                                }
                                const [,actualType,,] = actual
                                return baseType.convertFrom(actualType, makeActual(norm(name)))
                            }
                            return norm(name)
                        })
                        extendExpr += `(${extendParamExprs.join(", ")})`
                    }

                    if (this.baseTraits.length > 0) {
                        extendExpr += ` with ${this.baseTraits.map(trait => trait.name).map(norm).join(" with ")}`
                    }
                } else {
                    extendExpr = withExpr
                }
                const argExpr = argListExpr.length > 0 ? `(${argListExpr})` : ""
                writer.println(`sealed ${isAbstract? "abstract " : ""}class ${name}${argExpr}${extendExpr} {`)
                const classWriter = writer.indented()
                if (this.isAbstract) {
                    classWriter.println("def toJSON: Js.Value")
                } else {
                    classWriter.println(`${base? "override " : ""}def toJSON: Js.Value = Js.Obj(`)
                    const toJSONWriter = classWriter.indented()
                    const argWriter = toJSONWriter.indented()
                    if (discriminantEntry && discriminantEntry instanceof LiteralEntry && typeof discriminantEntry.value === "string") {
                        argWriter.println(`"${discriminant}" -> Js.Str("${discriminantEntry.value}")${params.length > 0? "," : ""}`)
                    }
                    params.forEach((p, idx, self) => {
                        const [name, type, own, mode] = p
                        if (mode !== "overridden") {
                            argWriter.println(`"${name}" -> ${type.toExpr(`this.${toName(p)}`)}${idx === self.length-1? "" : ","}`)
                        }
                    })
                    argWriter.end()
                    toJSONWriter.println(")")
                    toJSONWriter.end()
                }
                classWriter.end()
                writer.println("}")
                writer.println()
            }

            // build companion object
            writer.println(`object ${norm(name)} {`)
            const objWriter = writer.indented()
            let hasApplyOrUnapply = false
            if (!isAbstract) {
                // apply
                const propTypesExpr = realParams.map(toTypedName).join(", ")
                const propNamesExpr = realParams.map(toName).join(", ")

                objWriter.println(`def apply(${propTypesExpr}): ${norm(name)} = new ${norm(name)}(${propNamesExpr})`)
                hasApplyOrUnapply = true
            }
            if (hasOwnParams) {
                // unapply
                const wrap = ownParams.length > 1? (str: string) => `(${str})` : (str: string) => str
                const param = norm(beginLower(norm(name)))
                const accessExpr = wrap(ownParams.map(toName).map(prop => `${param}.${prop}`).join(", "))
                const propTypeTuple = wrap(ownParams.map(toType).join(", "))
                objWriter.println(`def unapply(${param}: ${norm(name)}): Option[${propTypeTuple}] = Some(${accessExpr})`)
                hasApplyOrUnapply = true
            }

            const hasFromJson = options.suppressFromJson.indexOf(this.name) === -1
            if (hasApplyOrUnapply && hasFromJson) {
                objWriter.println()
            }


            if (hasFromJson) {
                // parse (fromJson)

                function printInheritingCases(of: InterfaceEntry, caseWriter: Writer, obj: string, src: string) {
                    for (const intf of of.allInheritingIntf) {
                        const discriminantEntry = Properties.get(intf.props, discriminant)
                        if (!intf.isAbstract &&
                                discriminantEntry instanceof LiteralEntry &&
                                typeof discriminantEntry.value === "string") {
                            const cond = options.parseConditions[intf.name]
                            if (typeof cond === "boolean" && !cond) {
                                continue
                            }
                            const conditionExpr = (typeof cond === "function" ? `if ${cond(`${obj}`)} `: "")
                            const discriminantValue = discriminantEntry.value
                            caseWriter.println(`case "${discriminantValue}" ${conditionExpr}=> ${intf.fromExpr(src)}`)
                        }
                    }
                }

                if (!isAbstract) {

                    objWriter.println(`def from(src: Js.Value): ${norm(name)} = {`)
                    const fromWriter = objWriter.indented()
                    fromWriter.println("val _obj = src.obj")

                    if (!this.isInnerClass && this.needDiscriminant || discriminantEntry) {
                        if (!(discriminantEntry instanceof LiteralEntry) || typeof discriminantEntry.value !== "string") {
                            throw new Error(`Non abstract class '${norm(name)}' must have a specific string property '${discriminant}'`)
                        }
                        const discriminantValue = discriminantEntry.value


                        if (this.allInheritingIntf.length > 0) {
                            fromWriter.println(`_obj("${discriminant}").str match {`)
                            const caseWriter = fromWriter.indented()
                            printInheritingCases(this, caseWriter, "_obj", "src")
                            caseWriter.println("case _ =>")
                            caseWriter.end()
                            fromWriter.println("}")
                        }
                        fromWriter.println(`assert(_obj("${discriminant}").str == "${discriminantValue}")`)

                        const cond = options.parseConditions[this.name]
                        if (typeof cond === "function") {
                            fromWriter.println(`assert(${cond("_obj")})`)
                        }
                    }
                    fromWriter.println()

                    const hasParams = realParams.length > 0
                    fromWriter.println(`new ${norm(name)}${hasParams? "(" : ""}`)
                    const argWriter = fromWriter.indented()
                    realParams.forEach(([arg, entry,], idx, self) => {
                        const isLast = idx == self.length - 1
                        const value = `_obj("${arg}")`
                        const optValue = `_obj.get("${arg}")`
                        const svalue = entry.fromExpr(value, optValue)
                        argWriter.println(svalue + (isLast? "" : ","))
                    })
                    argWriter.end()
                    if (hasParams) {
                        fromWriter.println(")")
                    }
                    fromWriter.end()
                    objWriter.println("}")
                } else {
                    objWriter.println(`def from(src: Js.Value): ${norm(name)} = src("${discriminant}").str match {`)
                    const caseWriter = objWriter.indented()
                    printInheritingCases(this, caseWriter, "src.obj", "src")
                    caseWriter.println(`case discriminant => throw new IllegalArgumentException(s"Unknown ${discriminant} '$discriminant' for ${name}")`)
                    caseWriter.end()
                    objWriter.println("}")
                }
            }

            objWriter.end()
            writer.println("}")
            writer.println()
            writer.end()
        }

        asScalaType(): string {
            return norm(this.name)
        }

        fromExpr(src: string): string {
            return `${norm(this.name)}.from(${src})`
        }

        toExpr(self: string): string {
            return `${self}.toJSON`
        }

        _isSubtypeOf(other: Entry): boolean {
            if (other instanceof InterfaceEntry) {
                return this.allBases.indexOf(other) >= 0
            }
            return false
        }
    }

    class EnumEntry extends Definition {
        constructor(public readonly name: string,
                    public readonly org: Spec.Enum) {
            super()
        }

        @lazy
        get values(): string[] {
            return this.org.values.map(value => {
                if (typeof value !== "string") {
                    throw new Error("Can only process enums containing strings")
                }

                return value
            })
        }

        @lazy
        get inners(): Entry[] {
            return []
        }

        printDefinition(targetWriter: Writer): void {
            console.log("Print definition for enum ", this.name)
            const name = norm(this.name)
            const writer = targetWriter.sub()
            writer.println(`sealed abstract class ${name} {`)
            const classWriter = writer.indented()
            classWriter.println("def toJSON: Js.Str")
            classWriter.end()
            writer.println("}")
            writer.println()
            writer.println(`object ${name} {`)
            const innerWriter = writer.indented()
            this.values.forEach(value => {
                innerWriter.println(`final case object \`${norm(value)}\` extends ${name} {`)
                const objWriter = innerWriter.indented()
                objWriter.println(`override def toJSON: Js.Str = Js.Str("${value}")`)
                objWriter.println(`override def toString: String ="${name}[${value}]"`)
                objWriter.end()
                innerWriter.println("}")
                innerWriter.println()
            })
            innerWriter.println()
            // parse
            innerWriter.println(`def from(src: Js.Value): ${name} = src.str match {`)
            const caseWriter = innerWriter.indented()
            this.values.forEach(value => {
                caseWriter.println(`case "${value}" => \`${norm(value)}\``)
            })
            caseWriter.end()
            innerWriter.println("}")
            innerWriter.end()
            writer.println("}")
            writer.println()
            writer.end()
        }

        asScalaType(): string {
            return norm(this.name)
        }

        fromExpr(src: string): string {
            return `${this.asScalaType()}.from(${src})`
        }

        toExpr(self: string): string {
            return `${self}.toJSON`
        }

        _isSubtypeOf(other: Entry): boolean {
            return other === this
        }
    }

    // register native types
    const nativeTypes = [
        {
            name: "number",
            native: "Int",
            fromExpr: (src: string) => `${src}.num.toInt`,
            toExpr: (self: string) => `Js.Num(${self})`
        },
        {
            name: "string",
            native: "String",
            fromExpr: (src: string) => `${src}.str.toString`,
            toExpr: (self: string) => `Js.Str(${self})`
        },
        {
            name: "boolean",
            native: "Boolean",
            fromExpr: (src: string) => `${src}.isInstanceOf[Js.True.type]`,
            toExpr: (self: string) => `(if (${self}) Js.True else Js.False)`
        }
    ]
    for (const {name, native, fromExpr, toExpr} of nativeTypes) {
        definitions.set(name, new NativeEntry(name, native, fromExpr, toExpr))
    }

    // register all inferfaces and enums
    const defs = Object.entries(spec).map(([name, def]) => {
        const entry = definitionToEntry(def, name)
        definitions.set(name, entry)
        return entry
    })
    state.refsResolvable = true

    const intfs = defs.filter((def: Definition): def is InterfaceEntry => def instanceof InterfaceEntry)

    intfs.forEach(intf => intf.addToBaseIntf())
    state.basesMarked = true
    
    var globalWriter = new Writer()
    globalWriter.println(`package ${options.package}`)
    globalWriter.println()
    globalWriter.println("import upickle.Js")
    globalWriter.println()
    globalWriter.println()
    var typeWriter = globalWriter.sub()

    defs.forEach(def => def.printDefinition(typeWriter))

    typeWriter.end()
    globalWriter.end()

    return globalWriter.toString()
}