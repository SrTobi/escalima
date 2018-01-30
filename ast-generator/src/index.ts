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
function esspec(file :string, ...deps: SpecSource[]): SpecSource {
    return spec(`${estree}/${file}`)
}
function fixspec(file :string, ...deps: SpecSource[]): SpecSource {
    return spec(`${fixdir}/${file}`, ...deps)
}
const es5 = esspec("es5.md")
const es2015 = esspec("es2015.md", es5)
const es2016 = esspec("es2016.md", es2015)
const es2017 = esspec("es2017.md", es2016)
const es2018 = esspec("es2018.md", es2017)

const es5Fix = fixspec("fix-es5.md", es5)

const options: Options = {
    package: "escalima.ast",
    forceClasses: ["Node", "Literal", "Function", "NonAbstract"],
    forceTrait: [],
    forceAbstract: ["Node", "Literal"],
    discriminant: "type",
    renames: [{
        from: "object",
        to: "obj"
    }],
    parseConditions: {
        Directive: (src) => `${src}.contains("directive")`,
        ExpressionStatement: (src) => `!${src}.contains("directive")`,
        RegExpLiteral: (src) => `${src}.contains("regex")`,
        StringLiteral: (src) => `${src}("value").isInstanceOf[Js.Str] && !${src}.contains("regex")`,
        NumberLiteral: (src) => `${src}("value").isInstanceOf[Js.Num]`,
        NullLiteral: (src) => `${src}("value").isInstanceOf[Js.Null.type]`,
        BooleanLiteral: (src) => `${src}("value").isInstanceOf[Js.True.type] || ${src}("value").isInstanceOf[Js.False.type]`,
        Test: (src) => `${src}.contains("prop2")`
    }
}



async function main() {
    const test = { file: `${root}/test.md` }
    const test2 = { file: `${root}/test2.md`, deps: [test]}
    const spec = await importSpecs([es5Fix])
    console.log("Building scala ast")
    const result = await buildScalaAst(spec, options)
    console.log(`Writing ast to ${normalize(target)}`)
    await writeFile(target, result)
    console.log("Done.")
}

main()
    .then(() => process.exit(0))
