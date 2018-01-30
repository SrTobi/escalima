const {defineProperty, getPrototypeOf} = Object;

export default function nonrec<T = undefined, R = void>(defaultValue?: R | ((arg: T) => R)) {
    
    if (arguments.length == 0) {
        defaultValue = (() => {}) as any
    }

    return function (target: Object,
                     name: PropertyKey,
                     desc: TypedPropertyDescriptor<(arg: T) => R>): any {
        const org = desc.value
        const sym = Symbol("already called symbol")

        if (typeof org !== "function") {
            throw new Error("annotated member has to be a function")
        }

        desc.value = function(this: any): R {
            if (this[sym]) {
                if (typeof defaultValue == "function") {
                    return defaultValue.apply(this, arguments)
                } else if (typeof defaultValue !== "undefined") {
                    return defaultValue
                } else {
                    throw new Error("Missing default value")
                }
            }
            this[sym] = true
            try {
                return org.apply(this, arguments)
            } finally {
                this[sym] = false
            }
        }
    }
}