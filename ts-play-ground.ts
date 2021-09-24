try {
  const person: { age: number } = { age: 10 };
  const readonlyPerson: { readonly age: number } = person;

  person.age = 11;

  console.log(readonlyPerson); //?
} catch (error) {}
try {
  function func<T>(param: T) {}
  function func4<T>(param: T extends 'type' ? never : T) {}
  function func1<T extends string>(param: T) {}
  function func2<T extends string>(param: Exclude<T, 'type'>) {}
  function func3(param: string) {}
  let foo = 'str';
  const bar = 'str';
  func('a');
  func(foo);
  func(bar);
  func<'b'>('b');

  func4('type');
  func4(<const>'type');

  func3('a');
  func3(foo);
  func3(bar);

  func1(foo);
  func1(bar);
  func1('asdf');

  func2('asdf');
  func2('type');
} catch (error) {}
try {
  // type Exclusion = 'Invalid';
  // type ExcludeString = Exclude<string, Exclusion>;

  function func<T extends string>(param1: Exclude<T, 'type'>): void {}

  func('SS');
  func('type'); // error
} catch (error) {}
try {
  interface Foo {
    x: number;
    y: string;
  }
} catch (error) {}
try {
  type Foo<T extends string> = {
    type: string;
    // [key in T]: number;
  };

  type Bar<T extends string> = {
    [key in T]: string;
  } & { didi: number };

  function acceptDidiAndName(bar: Bar<'name'>) {}

  acceptDidiAndName({ didi: 1, name: 'birudo', age: 1 });
  acceptDidiAndName({ didi: 1 });
} catch (error) {}
try {
  let time = 0;
  const sayHi = () => {
    console.log(time++);
  };

  const callMe = () => {
    const arr = [sayHi()];
  };

  callMe();
  callMe();
} catch (error) {}
try {
  console.log(/[\[\{\(]/.test('['));
  console.log('['.match(/[\[\{\(]/));
  console.log('({'.search(/[\[\{\(]/));
} catch (error) {}
try {
  const foo = function (...args) {
    if (this instanceof foo) {
      this.context = 'new create object';
    }
    console.log(this);
    console.log(...args);
  };

  const bar = foo.bind({ context: 'bound this' }, 'bound args');
  new bar('calling args');
} catch (error) {}
try {
  const foo = function (this: unknown) {
    console.log(this);
  }.call();
} catch (error) {}
try {
  function call(fun: AnyFunction) {
    return fun();
  }
  class Foo {
    arrSayThis: () => void;
    normalSayThis: () => void;
    constructor() {
      this.arrSayThis = () => console.log('arrSayThis:', this);

      this.normalSayThis = function () {
        console.log('normalSayThis:', this);
      };

      call(this.arrSayThis);
      call(this.normalSayThis);
    }
  }

  const foo = new Foo();
} catch (error) {}
try {
  // handwrite bind
  const foo = function () {
    console.log('target function');
  }.bind({ name: 1 }, 1, 2, 3);
  console.log(1);
} catch (error) {}
try {
  // handwrite bind don't consider the situation use new keyword
  type AnyFunction = (...args: unknown[]) => unknown;
  interface Function {
    myBind: <T>(context: any, ...args: T[]) => AnyFunction;
  }

  Function.prototype.myBind = function (
    this: AnyFunction,
    context: any,
    ...args: unknown[]
  ) {
    const fn = this;

    return function (this: unknown, ...bindArgs: unknown[]) {
      return fn.call(context ?? this, ...args, ...bindArgs);
    };
  };

  const bar = {
    age: 12,
  };

  function foo(this: unknown, ...args: unknown[]): unknown {
    console.log('this:', this);
    console.log('args:', args);
    return args;
  }

  const boundFoo = foo.myBind(bar);
  boundFoo(1, 2, 3, 4);
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
