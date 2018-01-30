const {defineProperty, getPrototypeOf} = Object;

export default function lazy(this: any, target: any, name: PropertyKey, {get: initializer, enumerable, configurable, set: setter}: PropertyDescriptor={}): any {
    const {constructor} = target;
    if (initializer === undefined) {
        throw `@lazy can't be set as a property \`${name}\` on ${constructor.name} class, using a getter instead!`;
    }
    if (setter) {
        throw `@lazy can't be annotated with get ${name}() existing a setter on ${constructor.name} class!`;
    }

    function set(this: any, that: any, value: any) {
        defineProperty(that, name, {
            enumerable: enumerable,
            configurable: configurable,
            value: value
        });
        return value;
    }

    return {
        get(){
            if (this === target) {
                return initializer;
            }
            //note:subclass.prototype.foo when foo exists in superclass nor subclass,this will be called
            if (this.constructor !== constructor && getPrototypeOf(this).constructor === constructor) {
                return initializer;
            }
            return set(this, initializer.call(this));
        },
        set
    };
}