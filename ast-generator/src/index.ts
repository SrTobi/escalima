import {readSpec, importSpecs, SpecSource} from './importer'
import {buildScalaAst, Options} from './scala-exporter'
import {writeFile} from './fs'
import {basename, normalize} from 'path'


const root = `${__dirname}/..`
const estree = `${root}/estree`
const fixdir = `${root}/estree-fixes`

const target = `${root}/../escalima/shared/src/main/scala/escalima/ast/Nodes.scala`
// estree specs
function spec(file :string, ...deps: SpecSource[]): SpecSource {
    return {
        file: file,
        name: basename(file),
        deps
    }
}
function esspec(file: string, ...deps: SpecSource[]): SpecSource {
    return spec(`${estree}/${file}`, ...deps)
}
function fixspec(file: string, ...deps: SpecSource[]): SpecSource {
    return spec(`${fixdir}/${file}`, ...deps)
}

interface FixOptions {
    spec: SpecSource
    forceClasses: string[]
    forceTrait: string[]
    forceAbstract: string[]
    suppressFromJson: string[]
    renames: { from: string, to: string }[]
    parseConditions: { [name: string]: (src: string) => string }
}
function mergeFixOptions(org: FixOptions, ext: FixOptions): FixOptions {
    return {
        spec: ext.spec,
        forceClasses: org.forceClasses.concat(ext.forceClasses),
        forceTrait: org.forceTrait.concat(ext.forceTrait),
        forceAbstract: org.forceAbstract.concat(ext.forceAbstract),
        suppressFromJson: org.suppressFromJson.concat(ext.suppressFromJson),
        renames: org.renames.concat(ext.renames),
        parseConditions: Object.assign({}, org.parseConditions, ext.parseConditions)
    }
}

const es5 = esspec("es5.md")
const es2015 = esspec("es2015.md", es5)
const es2016 = esspec("es2016.md", es2015)
const es2017 = esspec("es2017.md", es2016)
const es2018 = esspec("es2018.md", es2017)

// es 5
const es5Fix = fixspec("fix-es5.md", es5)
const es5FixOptions: FixOptions = {
    spec: es5Fix,
    forceClasses: [
        "Node",
        "Literal",
        "Function",
        "NonAbstract",
        "ForInStatement",
    ],
    forceTrait: [],
    forceAbstract: [ "Node", "Literal" ],
    renames: [{
        from: "object",
        to: "obj"
    }],
    suppressFromJson: [ "Node" ],
    parseConditions: {
        Directive: (src) => `${src}.contains("directive")`,
        ExpressionStatement: (src) => `!${src}.contains("directive")`,
        RegExpLiteral: (src) => `${src}.contains("regex")`,
        StringLiteral: (src) => `${src}("value").isInstanceOf[Js.Str] && !${src}.contains("regex")`,
        NumberLiteral: (src) => `${src}("value").isInstanceOf[Js.Num]`,
        NullLiteral: (src) => `${src}("value").isInstanceOf[Js.Null.type]`,
        BooleanLiteral: (src) => `${src}("value").isInstanceOf[Js.True.type] || ${src}("value").isInstanceOf[Js.False.type]`,
    }
}

// es 2015
const es2015Fix = fixspec("fix-es2015.md", es2015, es5Fix)
const es2015FixOptions: FixOptions = mergeFixOptions(es5FixOptions, {
    spec: es2015Fix,
    forceClasses: [
        "Class",
        "ModuleSpecifier",
        "ImportSpecifier",
    ],
    forceTrait: [],
    forceAbstract: [ 
        "ModuleSpecifier"
    ],
    renames: [
        {
            from: "class",
            to: "clazz"
        }
    ],
    suppressFromJson: [],
    parseConditions: {}
})

// es 2016
const es2016Fix = fixspec("fix-es2016.md", es2016, es2015Fix)
const es2016FixOptions: FixOptions = mergeFixOptions(es2015FixOptions, {
    spec: es2016Fix,
    forceClasses: [],
    forceTrait: [],
    forceAbstract: [],
    renames: [],
    suppressFromJson: [],
    parseConditions: {}
})

// es 2017
const es2017Fix = fixspec("fix-es2017.md", es2017, es2016Fix)
const es2017FixOptions: FixOptions = mergeFixOptions(es2016FixOptions, {
    spec: es2017Fix,
    forceClasses: [],
    forceTrait: [],
    forceAbstract: [],
    renames: [],
    suppressFromJson: [],
    parseConditions: {}
})

// es 2018
const es2018Fix = fixspec("fix-es2018.md", es2018, es2017Fix)
const es2018FixOptions: FixOptions = mergeFixOptions(es2017FixOptions, {
    spec: es2018Fix,
    forceClasses: [],
    forceTrait: [],
    forceAbstract: [],
    renames: [],
    suppressFromJson: [],
    parseConditions: {}
})


const options: Options & FixOptions = Object.assign({
    package: "escalima.ast",
    discriminant: "type",
}, es2018FixOptions)



async function main() {
    const spec = await importSpecs([options.spec])
    console.log("Building scala ast")
    const result = await buildScalaAst(spec, options)
    console.log(`Writing ast to ${normalize(target)}`)
    await writeFile(target, result)
    console.log("Done.")
}

main()
    .then(() => process.exit(0))
