try {
  // Extend
  // This solution is lack of validation.
  function myExtend(Sub: Constructor, Super: Constructor): Constructor {
    const res = function () {
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
