try {
  function SuperClass(this: any) {
    this.propertyOnSuperInstance = 'hey';
  }
  SuperClass.prototype = {
    propertyOnSuperProto: 'hello',
  };

  function SubClass(this: any) {
    SuperClass.call(this);
    this.propertyOnSuperInstance = 'hey';
  }

  SubClass.prototype = Object.create(SuperClass.prototype);
  Object.assign(SubClass.prototype, {
    propertyOnSubProto: 'didi',
  });

  type Constructor = {
    new (...parameters: any[]): void;
  };
  function isSubClass(Super: Constructor, Sub: Constructor) {
    it('should ', () => {
      const sup = new Super();
      const sub = new Sub();

      expect(sub).toMatchObject(sup);
    });
  }
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
