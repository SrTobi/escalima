export default function once(target: Object,
                             name: PropertyKey,
                             desc: TypedPropertyDescriptor<() => void>): any {
    const org = desc.value
    const sym = Symbol("once called symbol")

    if (typeof org !== "function") {
        throw new Error("annotated member has to be a function")
    }

    desc.value = function(this: any): void {
        if (!this[sym]) {
            this[sym] = true
            org.apply(this)
        }
    }
}