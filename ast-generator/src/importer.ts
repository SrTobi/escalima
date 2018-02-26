import { parse as parseSpec } from './grammar'
import { lexer as lexMarkdown } from 'marked'
import * as Spec from './spec'
import {readFile} from './fs'

import Specification = Spec.Specification
import { basename } from 'path';


export async function readSpec(file: string): Promise<Specification> {
    const content = await readFile(file)
    const mdTokens = await lexMarkdown(content)
    const onlyCodeBlocks = (token: marked.Token): token is marked.Tokens.Code => token.type === 'code'
    const codes = mdTokens.filter(onlyCodeBlocks).map(token => token.text)
    if (codes.length === 0) {
        console.warn(`${basename(file)} did not contain any markdown code blocks`)
    }
    const specText = codes.join("\n")
    return parseSpec(specText)
}


export function extendSpec(base: Specification, extension: Specification): Specification {
    const result = Object.assign({}, base) as Specification;
	for (const name in extension) {
		const item = extension[name];
		if (item.kind === 'interface' && item.extend) {
            const baseItem = base[name];
            if (!baseItem) {
                throw new Error(`Can not extend ${name} because it wasn't defined`)
            }
            if (baseItem.kind !== "interface") {
                throw new Error("Can not merge interface and " + baseItem.kind)
            }
            result[name] = Spec.extendInterface(baseItem, item)
		} else {
			result[name] = item;
		}
	}
    return result;
}

export type SpecSource = string | {
    name?: string
    file: string
    deps?: SpecSource[]
}

interface NormalizeSpecSource {
    name: string
    file: string
    depNames: string[]
}

export async function importSpecs(specs: SpecSource[]): Promise<Specification> {
    const reg = new Map<string, NormalizeSpecSource>()

    function register(s: SpecSource): string {
        if (typeof s === "string") {
            var name = s
            var file = s
            var deps: string[] = []
        } else {
            var name = s.name || s.file
            var file = s.file
            var deps = (s.deps || []).map(register)
        }

        const existing = reg.get(name)
        if (existing && existing.file !== file) {
            throw new Error(`${name} found twice with different file`)
        }

        reg.set(name, {
            name: name,
            file: file,
            depNames: deps,
        })
        return name
    }
    specs.forEach(register)

    const orderedSpecs: NormalizeSpecSource[] = []

    function resolve(source: SpecSource): NormalizeSpecSource {
        if (typeof source === "string") {
            var name = source
        } else {
            var name = source.name || source.file
        }

        const resolved = reg.get(name)
        if (!resolved) {
            throw new Error("Unresolved dependency")
        }
        return resolved
    }

    function remove(spec: NormalizeSpecSource): void {
        const i = orderedSpecs.findIndex(s => s.name == spec.name)
        if (i >= 0)
            orderedSpecs.splice(i, 1)
    }


    function add(spec: NormalizeSpecSource): void {
        remove(spec)
        orderedSpecs.unshift(spec)
        // reverse dependencies because deps from further back should be further back in the orderedSpecs list
        spec.depNames.reverse().forEach(dep => {
            add(resolve(dep))
        })

    }

    specs
        .reverse()
        .forEach(s => add(resolve(s)))

    let result: Specification = {}
    for (const spec of orderedSpecs) {
        console.log(`Load spec ${spec.name}...`)
        result = extendSpec(result, await readSpec(spec.file))
    }
    return result
}
