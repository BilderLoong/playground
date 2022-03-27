try {
  // Bind, call, apply
  Function.prototype.myCall = function (obj: any = globalThis, ...args: any[]) {
    const fn = this;
    const sym = Symbol();
    obj[sym] = fn;

    const res = obj[sym](...args);
    delete obj[sym];

    return res;
  };

  const obj1 = { age: 12 };
  console.log(log.myCall(obj1, 'name', 'name2'));

  Function.prototype.myBind = function (
    obj: any = globalThis,
    ...preArgs: any[]
  ): AnyFunction {
    const fn = this;
    const sym = Symbol();
    obj[sym] = fn;

    return function (...postArgs: any[]) {
      obj[sym](...preArgs, ...postArgs);
      delete obj[sym];
    };
  };

  // Test
  function log(this: any, name: string, name2: string) {
    console.log({
      name,
      name2,
      age: this.age,
    });

    return 'return value of log';
  }

  const obj = {
    age: 11,
  };

  const res = log.myBind(obj, 'name');
  res('name2');
  console.log(obj);
} catch (error) {}
try {
  // `new` operator
  const con1 = function (this: { color: string }, color: string) {
    this.color = color;
  } as any as Constructor;
  const myNew = (con: Function, ...args: any[]): object => {
    const obj = Object.create(con.prototype);
    const res = con.apply(obj, args);

    return Object(res) === res || typeof res === 'function' ? res : obj;
  };

  const res = myNew(con1, 'red');
  const res1 = new con1('red');

  it('should ', () => {
    expect(res).toEqual(res1);
  });
} catch (error) {}
try {
  // Extend
  // This solution is lack of validation.
  function myExtend(Sub: Constructor, Super: Constructor): Constructor {
    const res = function (this: any) {
      Super.call(this);
      Sub.call(this);
    } as any as Constructor;

    Object.assign(Object.create(Super), Sub.prototype);

    return res;
  }
} catch (e) {}
try {
  // Object.create()
  function create(obj: object) {
    const F = function () {} as any as Constructor;
    F.prototype = obj;

    return new F();
  }
} catch (error) {}

try {
  // Inherit
  const SuperClass = function (this: any) {
    this.propertyOnSuperInstance = 'hey';
  } as any as Constructor;

  SuperClass.prototype = {
    propertyOnSuperProto: 'hello',
  };

  (function () {
    // Prototype Chaining
    const SubClass = function (this: any) {
      this.propertyOnSupInstance = 'hey';
    } as any as Constructor;
    SubClass.prototype = new SuperClass();

    const sub = new SubClass();
    console.log(sub);
    console.log(sub instanceof SuperClass);
  })();

  (function () {
    // Constructor Stealing
    const SubClass = function (this: any) {
      SuperClass.call(this);
      this.propertyOnSupInstance = 'hey';
    } as any as Constructor;

    const sub = new SubClass();
    console.log(sub);
    console.log(sub instanceof SuperClass);
  })();

  (function () {
    // Combination Inheritance
    const SubClass = function (this: any) {
      SuperClass.call(this);
      this.propertyOnSupInstance = 'hey';
    } as any as Constructor;
    SubClass.prototype = new SuperClass();

    const sub = new SubClass();
    console.log(sub);
    console.log(sub instanceof SuperClass);
  })();

  (function () {
    // Parasitic Combination Inheritance
    const SubClass = function (this: any) {
      SuperClass.call(this);
      this.propertyOnSupInstance = 'hey';
    } as any as Constructor;

    const proto = Object.create(SuperClass.prototype);
    // Whether change the `constructor` property doesn't affect the result of `sub instanceof SuperClass`.
    Object.defineProperties(proto, {
      constructor: { value: SubClass, enumerable: false, writeable: false },
    });
    SubClass.prototype = proto;

    const sub = new SubClass();
    console.log(sub);
    const con = sub.constructor;
    console.log(con);
    console.log(sub instanceof SuperClass);
    console.log(sub instanceof SubClass);
  })();
} catch (error) {}

try {
  // instanceof
  function myInstanceof1(obj: any, constructor: Function): boolean {
    return constructor.prototype.isPrototypeOf(obj);
  }

  function myInstanceof0(obj: any, constructor: Function): boolean {
    let proto = Object.getPrototypeOf(obj);

    while (proto) {
      if (proto === constructor.prototype) {
        return true;
      }

      proto = Object.getPrototypeOf(proto);
    }

    return false;
  }

  class C1 {
    constructor() {}
  }
  const c1 = new C1();

  it('should ', () => {
    expect(myInstanceof1(c1, C1)).toBe(true);
    expect(myInstanceof1(c1, String)).toBe(false);
    expect(myInstanceof1(new Date(), C1)).toBe(false);
  });

  it('should ', () => {
    expect(myInstanceof0(c1, C1)).toBe(true);
    expect(myInstanceof1(c1, String)).toBe(false);
    expect(myInstanceof0(new Date(), C1)).toBe(false);
  });
} catch (error) {}

(function () {
  // Compose function;
  // https://segmentfault.com/a/1190000008394749
  (function () {
    // Compose with reduce.
    const compose_0 =
      (...fns: Function[]) =>
      (x: any) =>
        fns.reduceRight((pre, fn) => fn(pre), x);

    it('should ', () => {
      const double = (x: number) => x * 2;
      const minus2 = (x: number) => x - 2;
      expect(compose_0(double, minus2)(10)).toBe(16);
    });
  })();

  (function () {
    // Compose use recursion.
    const compose = (...fns: Function[]) => {
      let cur = fns.length - 1;
      let res: any;
      return function f1(this: any, ...args: any[]): any {
        if (cur >= 0) {
          res = fns[cur--].apply(this, args);
          return f1.call(this, res);
        } else {
          cur = fns.length - 1;
          return res;
        }
      };
    };

    it('should ', () => {
      const double = (x: number) => x * 2;
      const minus2 = (x: number) => x - 2;

      expect(compose(double, minus2)(10)).toBe(16);
    });
  })();

  (function () {
    // Compose use iteration.
    const compose = (...fns: Function[]) => {
      const length = fns.length;
      if (fns.every((e) => typeof e !== 'function')) {
        throw new TypeError('Expect function');
      }

      return function (this: any, ...args: any[]) {
        let i = fns.length - 1;
        // When the fns is empty, return the first argument.
        let res = length ? fns[i].apply(this, args) : args[0];
        while (i--) {
          res = fns[i].call(i, res);
        }

        return res;
      };
    };

    it('should ', () => {
      const double = (x: number) => x * 2;
      const minus2 = (x: number) => x - 2;

      expect(compose(double, minus2)(10)).toBe(16);
    });
  })();
})();
