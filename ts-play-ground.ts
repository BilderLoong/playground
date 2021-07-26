try {
  function dealDishInfo(menuSku, cartSku) {
    if (!menuSku || !cartSku) return null;
    // 去掉 sku 级别失效商品 (无||不在可售时间||售完)
    if (!menuSku || !menuSku.validity || menuSku.soldOut) return null;

    // 校验菜品信息
    if (menuSku.skuName !== cartSku.skuName) return null;
    // the below li is difference.
    if (menuSku.currentPrice !== cartSku.currentPrice) return null;
    if (menuSku.originalPrice !== cartSku.originalPrice) return null;
    if (menuSku.memberPrice !== cartSku.memberPrice) return null;
    if (menuSku.dishType !== cartSku.dishType) return null;
    if (menuSku.canWeight !== cartSku.canWeight) return null;
    if (menuSku.unit !== cartSku.unit) return null;

    // 有规格时才判断
    if (menuSku.specAttrs && cartSku.specAttrs) {
      // TODO:如何判断规格信息一致
      let isEqual = true;
      // TODO: 代码有问题，此处不会走到false
      Object.keys(menuSku.specAttrs).forEach((key) => {
        if (menuSku.specAttrs[0][key] !== cartSku.specAttrs[0][key]) {
          isEqual = false;
        }
      });
      if (!isEqual) return null;
      // if (!Immutable.is(menuSku.specAttrs, cartSku.specAttrs)) return null;
    }

    // 更新必点菜属性(不存在必点规则时购物车存在老的必点菜时的场景)
    if (cartSku.mustCount >= 0) {
      const matchSku = mustDishInfo.find(
        (item) => item.skuId === cartSku.skuId
      );
      if (!matchSku) {
        cartSku = Immutable.set(cartSku, 'mustCount', -1);
      }
    }

    return cartSku;
  }

  const _ = require('underscore');
  const menuSku = {
    skuId: '671115260',
    spuId: '671115259',
    skuName:
      '这是一个超长的菜名：菜品价格超长时，有菜品折扣，加入购物车后，折扣后的菜品价格会被遮住',
    validity: true,
    saleTimeDesc: '',
    // note
    currentPrice: 499999.5,
    originalPrice: 999999,
    memberPrice: 899999,
    soldOut: false,
    stockCount: 3,
    canWeight: false,
    defaultSelected: false,
    dishType: 0,
    specAttrs: [
      {
        specificationId: -1,
        specificationName: '规格',
        valueId: 25072250,
        value: '',
      },
    ],
    mbDiscountTag: null,
    promotionTag: {
      labelCode: 0,
      labelName: null,
      extraInfo: null,
      type: 22,
      tag: '5折',
      tagDesc: null,
      tags: null,
    },
    promotionRule: {
      type: 22,
      actId: '2000016280545',
      status: 1,
      actName: '☁️的分类5折',
      periodLimited: false,
      startDate: '',
      endDate: '',
      timeLimit: null,
      rule: null,
    },
    tastes: null,
    methods: null,
    packageGroups: null,
    unit: '份',
    showRemark: false,
    includeMethodPrice: false,
    includeTastePrice: false,
    dishBoxVO: null,
    minFeeding: -1,
    maxFeeding: -1,
    allowManualDiscount: 20,
  };

  const cartSku = {
    spuId: '671115259',
    skuId: '671115260',
    skuName:
      '这是一个超长的菜名：菜品价格超长时，有菜品折扣，加入购物车后，折扣后的菜品价格会被遮住',
    canWeight: false,
    weight: null,
    count: 3,
    specAttrs: [
      {
        specificationId: -1,
        specificationName: '规格',
        valueId: 25072250,
        value: '',
      },
    ],
    remark: null,
    mustCount: -1,
    createTime: 1626955546952,
    recommendType: null,
    categoryId: '63327117',
    unit: '份',
    dishType: 0,
    // note
    currentPrice: 449999.5,
    originalPrice: 999999,
    memberPrice: 899999,
    soldOut: false,
    validity: true,
    stockCount: 3,
    saleTimeDesc: '',
    showRemark: false,
    promotionTag: {
      labelCode: 0,
      labelName: null,
      extraInfo: null,
      type: 22,
      tag: '5折',
      tagDesc: null,
      tags: null,
    },
    promotionRule: {
      type: 22,
      actId: '2000016280545',
      status: 1,
      actName: '☁️的分类5折',
      periodLimited: false,
      startDate: '',
      endDate: '',
      timeLimit: null,
      rule: null,
    },
    defaultSelected: false,
    packageGroups: null,
    includeMethodPrice: false,
    includeTastePrice: false,
    dishBoxVO: null,
    minFeeding: -1,
    maxFeeding: -1,
    allowManualDiscount: 20,
    methods: [],
    tastes: [],
    extraPrice: 0,
    goodsNo: 'a6b92da5471ddebdba318ad446cd0277',
    userAvatars: [
      {
        uid: '69237496998760826221',
        count: 3,
      },
    ],
  };
  const res1 = _.omit(menuSku, function (v, k) {
    return cartSku[k] === v;
  });
  console.log(res1);

  const res2 = dealDishInfo(menuSku, cartSku);
  console.log(res2);
  // hand write extends function

  function extend(sup: Function, sub: Function): Function {
    const proto = Object.create(sup.prototype);
    proto.constructor = sub;
    sub.prototype = proto;

    return function (this: unknown, ...args: Array<unknown>) {
      sup.apply(this, args);
      sub.apply(this, args);
    };
  }
} catch (error) {}
try {
  const res = JSON.stringify(() => 1);
  console.log(res);

  const res1 = JSON.stringify([1]);
  console.log(res1);
} catch (error) {}
try {
  const set1 = new Set([1]);
} catch (error) {}
try {
  function foo() {
    console.log(arguments.callee);
  }
  foo();
} catch (error) {}

try {
  (function () {
    foo();
    function foo({ a = 1, b = 2 }) {
      console.log(a, b);
    }
  })();
  (function () {
    const arr = [1, 2, 3];
    const [x, ...rest] = arr;
    console.log(rest);
  })();
  (function () {
    const arr = [1, 2, 3];

    const [x, , y] = arr;
    // const x = arr[0];
    // const y = arr[2];
    console.log(x, y);

    // temp = a;
    // a = b;
    // b = temp + b
  })();
  (function () {
    let a = 1,
      b = 2;
    [a, b] = [b, a + b];
    console.log(a, b); // 2, 3
    // temp = a;
    // a = b;
    // b = temp + b
  })();
  (function () {
    // RegEx
    const num = '123w435';
    const alph = 'asdfasdf';
    const numAndAlph = num + alph;

    const res1 = num.match(/.*(?<=.*[a-z])(?<=.*\d)/g);
    const res2 = alph.match(/.*(?<=.*[a-z])(?<=.*\d)/g);
    const res3 = numAndAlph.match(/.*(?<=.*[a-z])(?<=.*\d)/g);

    console.log();
  })();
  (function () {
    const arr = [];
    for (let i = 0; i < 10; i++) {
      arr.push(
        new Promise((resolve, reject) => {
          resolve('');
        })
      );
    }

    arr.push(Promise.reject());
    const res = Promise.all(arr)
      .then((val) => console.log(val))
      .catch((val) => console.log(val));
  })();
  (function () {
    function Maker() {
      this.name = 'foo';
      return null;
    }

    const obj = new Maker();
  })();
  (function () {
    class Foo {}

    class Bar extends Foo {
      constructor() {
        return {};
      }
      method() {}
    }

    const bar = new Bar();
    console.log(bar);
  })();
  // realize instanceof operator
  (function () {
    function instanceOf(l: object, r: object) {
      const proto = r.prototype;
      let curProto = Object.getPrototypeOf(l);
      while (curProto) {
        if (proto === curProto) return true;
        curProto = Object.getPrototypeOf(curProto);
      }

      return false;
    }

    class Foo {}
    class Bar {}

    const foo = new Foo();
    console.log(foo instanceof Foo);
    console.log(instanceOf(foo, Foo));
    console.log(instanceOf(foo, Object));
    console.log(instanceOf(foo, Bar));
  })();
  // in operator
  (function () {
    const obj = Object.defineProperty({}, 'foo', {
      value: 1,
      enumerable: false,
    });

    for (const key in obj) {
      console.log(key);
    }

    console.log('foo' in obj);
  })();

  //class
  (function () {
    const foo = {
      sayHi() {
        console.log('hi from foo');
      },
    };

    const bar = {
      sayHi() {
        super.sayHi();
      },
    };
    Object.setPrototypeOf(bar, foo);

    bar.sayHi();
  })();
  (function () {
    let animal = {
      name: 'Animal',
      eat() {
        // animal.eat.[[HomeObject]] == animal
        console.log(`${this.name} eats.`);
      },
    };

    let rabbit = {
      __proto__: animal,
      name: 'Rabbit',
      eat() {
        // rabbit.eat.[[HomeObject]] == rabbit
        super.eat();
      },
    };

    let longEar = {
      __proto__: rabbit,
      name: 'Long Ear',
      eat() {
        // longEar.eat.[[HomeObject]] == longEar
        super.eat();
      },
    };

    // works correctly
    longEar.eat(); // Long Ear eats.
  })();

  (function () {
    class Foo {
      sayHi() {
        console.log('hi');
      }
    }

    const foo = new Foo();
    const bar = {
      sayHi() {
        console.log('hi');
      },
    };

    console.log(bar.hasOwnProperty('sayHi'));
    console.log(foo.hasOwnProperty('sayHi'));
  })();
  (function () {
    class Base {
      constructor() {
        console.log('Base constructor run');
      }

      name = (function () {
        console.log('Base class field initialized');
      })();
    }

    class Derived extends Base {
      constructor() {
        super();
        console.log('Derived constructor run');
      }
      name = (function () {
        console.log('Derived class field initialized');
      })();

      age = 1;
    }

    const sub = new Derived();
  })();
  (function () {
    // What does the Foo.prototype contain ?
    class Foo {
      constructor() {
        this.name = 'bar';
      }
      sayName() {
        console.log(this.name);
      }
    }
    console.log(Foo.prototype);
  })();

  (function () {
    class Foo {
      say = () => console.log(this);
    }

    const foo = new Foo();

    const bar = {
      say: function () {
        console.log(this);
      },
    };

    bar.say();
    foo.say();

    console.log();
  })();

  (function () {
    const bar = {
      self: this,
    };

    console.log(bar.self);
  })();

  // const proto = {
  //   location: 'on the proto',
  // };

  // const person2 = Object.create(proto);
  // Object.defineProperty(person2, 'location', {
  //   enumerable: false,
  //   value: 'on the instance',
  // });

  // // person2.location = 'on the instance';

  // for (const key in person2) {
  //   if (Object.prototype.hasOwnProperty.call(person2, key)) {
  //     console.log(key);
  //   }
  // }

  // for (const key of Object.getOwnPropertyNames(person2)) {
  //   console.log(`${key}: ${person2[key]}`);
  // }

  // // Object.key()
  // // const protoOfFoo = {
  // //   location: 'on the proto',
  // // };

  // // const foo = Object.create(protoOfFoo);

  // // Object.defineProperties(foo, {
  // //   one: { enumerable: true, value: 1 },
  // //   two: { enumerable: false, value: 2 },
  // // });

  // // console.log(Object.keys(foo));
  // // console.log(Object.getOwnPropertyNames(foo));

  // class Parent {
  //   parentPro = 'parent';
  //   foo() {}
  // }

  // class Child extends Parent {
  //   childPro = 'child';
  //   bar() {}
  // }

  // const child = new Child();
  // console.log(child);

  // console.log(child.hasOwnProperty('parentPro')); // true
  // console.log(child.hasOwnProperty('foo'));
  // console.log(child.hasOwnProperty('bar'));

  // function Parent2() {
  //   this.parentPro = 'parent';
  // }
  // Parent2.prototype.foo = function () {};

  // function Child2() {
  //   Parent2();
  //   this.childPro = 'child';
  // }
  // Child2.prototype = new Parent2();

  // const child2 = new Child();
  // console.log(child2);

  // console.log(child2.hasOwnProperty('parentPro')); // true
  // console.log(child2.hasOwnProperty('foo'));
  // console.log(child2.hasOwnProperty('bar'));

  // combination inheritance

  function SuperType(name: string) {
    this.name = name;
    this.color = ['red'];
  }

  SuperType.prototype.sayName = function () {
    console.log(this.name);
  };

  function SubType(name: string, age: number) {
    SuperType.call(this, name);

    this.age = age;
  }

  SubType.prototype = new SuperType();

  const person = new SubType();

  for (const i in person) {
    console.log(i);
  }

  // parasitic inheritance

  const original = {};

  const res = Object.create(original);

  res.sayHi = function () {
    console.log('hi');
  };

  // parasitic combination inheritance
  (function () {
    function SuperType(this: any, name: string) {
      this.name = name;
      this.color = ['red'];
    }

    SuperType.prototype.sayName = function () {
      console.log(this.name);
    };

    function SubType(this: any, name: string, age: number) {
      SuperType.call(this, name);

      this.age = age;
    }

    // if direct assign the SuperType.prototype to the
    // SubType.prototype, then when the instance of
    // SubType access the constructor property will
    // get the SuperType instead SubType which isn't right behavior.
    // So we need copy a new prototype from the SuperType.prototype
    // And assign the SubType to the new prototype.constructor

    // wrong method
    // SubType.prototype = SuperType.prototype;

    // right method

    const newPrototype = Object.create(SuperType.prototype);
    newPrototype.constructor = SubType;
    SubType.prototype = newPrototype;

    const person = new SubType();

    console.log(person.constructor);

    for (const i in person) {
      console.log(i);
    }
  })();

  (function () {
    // compare the difference between Object.create() amd Object.assign()

    const obj = {
      a: 1,
    };

    const foo = Object.create(obj);
    const bar = Object.assign({}, obj);

    obj.a = 2;

    console.log(foo.a);
    console.log(bar.a);
  })();

  (function () {
    // realize Object.create() using two methods

    function object1(o) {
      const res = {};
      Object.setPrototypeOf(res, o);
      return res;
    }

    function object2(o) {
      function F() {}
      F.prototype = o;
      return new F();
    }

    const foo = { a: 1 };
    const obj1 = object1(foo);
    const obj2 = object2(foo);

    console.log(obj1);
  })();
} catch (error) {
  console.error(error);
}
