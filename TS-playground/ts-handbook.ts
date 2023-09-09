export {};
try {
  const what = (BaseClass: Function) => {
    console.log(`what?`);
    return class extends BaseClass {
      constructor() {
        super();
        console.log('log from what dec');
      }
    };
  };

  @what
  class Foo {
    constructor() {
      console.log('Foo');
    }
  }
  console.log(123);
} catch (e) {}
try {
  /**
    *
    * [123](https://google.com)
    *
    */
  enum RedirectCode {
    REDIRECT_TO_SHOP_LIST = 1200,
  }

  RedirectCode;
} catch (error) {}
try {
  function classDecorator(con: Function) {
    console.log(con);
  }

  @classDecorator
  class Foo {
    constructor() {
      console.log('Foo');
    }
  }
} catch (error) {
  console.log(error);
}
namespace dodo {
  export const foo = 1;
  export type didi = number;
  type baba = string;
}

namespace dodo {
  const bar: baba = '1';
}
try {
} catch (error) {}
try {
  // Declaration merging
  interface Foo {
    name: string;
  }
  interface Foo {
    name: number;
  }
} catch (error) {}
try {
  // Parameter Properties
  class test {
    public name: string;
    constructor(name: string) {
      this.name = name;
    }
  }

  const foo = new test('123');
  console.log(foo);
} catch (error) {}
try {
  class Foo {
    private a = 1;

    static bar() {
      console.log(this);
    }
  }
} catch (error) {}
try {
  interface Animal {
    dateOfBirth: any;
  }

  interface Dog extends Animal {
    breed: any;
  }

  class AnimalHouse {
    protected resident: Animal;
    private foo: number = 1;
    public d: number = 1;
    constructor(animal: Animal) {
      this.resident = animal;
    }
  }

  class DogHouse extends AnimalHouse {
    // Does not emit JavaScript code,
    // only ensures the types are correct
    declare resident: Dog;
    constructor(dog: Dog) {
      super(dog);
      this.resident = dog;
    }
  }

  const dog = new DogHouse({ breed: 1, dateOfBirth: 2 });
  const foo = dog.resident;
} catch (error) {}
try {
  class Base {
    name: string = 'base';
  }

  class Derived extends Base {
    name: number = 1;
    // (property) derived.name: number
    // 型 'derived' のプロパティ 'name' を基本データ型 'base' の同じプロパティに割り当てることはできません。
    // 型 'number' を型 'string' に割り当てることはできません。ts(2416)
  }
} catch (error) {}

try {
  // implement
  interface a {
    x: number;
    y?: number;
  }
  class c implements a {
    x = '1';
  }

  const foo = new c();
} catch (error) {}
try {
  // @strictpropertyinitialization: false
  class point {
    readonly x: number = 1;
    constructor() {
      this.x = 2;
    }
  }

  const p = new point(true);
  // p.x = '123';
  // p.y = 1;
  console.log(p);
} catch (error) {}

export const a = 1;

try {
  // WeakMap

  let obj1: object | null = { a: '1' };
  const wMap = new WeakMap([[obj1, 'didi']]);
  const res1 = wMap.get(obj1);
  console.log({ res1 });

  obj1 = null;
  console.log(wMap.get(obj1));
} catch (error) {}

try {
  (function () {
    interface person {
      name: string;
      age: number;
    }
    interface person {
      name: string;
      age: number;
    }
    type footype = {
      location: string[];
      middlename?: string; // optional properties
    };

    type bar = string | object; // union type and type aliases

    let anytype;
    let anumber: number = 0;
    //         ^ this is called type assignment
    let anumberarr = [1, 2, 3];
    //       ^  let anumberarr: number[]

    let anumberorstringarr: number | string[] = 1; // union type

    function afunctionwithstringreturn(name: string): string {
      console.log(`hello,${name}`);
      return `hello,${name}`;
    }

    [1, 2, 3].map((e, i) => e.tostring() + i.tostring()); // contextual typing
  });
} catch {}

(function () {
  // type assertions
  const stack = ['1', 1];
  let stacktop: string;
  stacktop = stack[0];
  stacktop = stack[0] as string;

  const myinputs = document.getelementsbyname(
    'input'
  ) as nodelistof<htmlinputelement>;

  // the below two type assertions have the same effect
  const myinput = document.getelementbyid('myinput') as htmlinputelement;
  const $myinput = <htmlinputelement>document.getelementbyid('myinput'); // can't be use in jsx

  const welcome = 'hello' as any as number;
  const $welcome = 'hello' as unknown as number;
  const _welcome = 'hello' as number;
});

//----------------------------------------------------------------

(function () {
  // literal types

  let changingstring = 'hello';
  changingstring = 'welcome';

  const conststring = 'hello';

  let foo: 'bar';
  foo = 'bar';
  foo = 'hello';

  let myname: 'birudo' | 'bilder' | 'cokoryuu';
  myname = 'birudo';
  myname = 'bilder';
  myname = 'blider';

  // the below two is equal
  let booleanliteraltype: boolean;
  let _booleanliteraltype: true | false;

  // combine literal with non-literal
  let literaltypewithnonliteraltype: person | 'bar';
  literaltypewithnonliteraltype = { name: 'huzils', age: 21 };
  literaltypewithnonliteraltype = 'bar';
  literaltypewithnonliteraltype = 'foo';

  // literal inference
  const handlerequest = (url: string, method: 'get' | 'post') => {
    console.log(`${url},${method}`);
  };

  const req1 = { url: 'url', method: 'get' };

  /* will get error, because the inference type of req.method is
 string type don't correspond to the function argument
 type(a union type) */
  handlerequest(req1.url, req1.method);

  // workaround 1
  handlerequest(req1.url, req1.method as 'get');

  // workaround 2
  const req2 = { url: 'url', method: 'get' as 'get' };
  handlerequest(req2.url, req2.method);

  // workaround 3
  const req3 = {
    url: 'url',
    method: 'get' as 'get',
  } as const;
  /*     ^ ensuring that all properties are assigned the literal type
  instead of a more general version like string or number */

  handlerequest(req3.url, req3.method);
});
// strictnullchecks on

function dosomething(x: string | null | undefined) {
  if (typeof x === 'string') {
    x.tolowercase(); //narrowing
  }
  x?.touppercase(); // non-null assertion operator

  x.touppercase();
}

function _dosomething(x: string | null | undefined) {
  if (typeof x === 'string') {
    return x.tolowercase(); // narrowing
  }

  x?.touppercase(); // this line is never got evaluated
  x.touppercase();
}

// never type
let x: null = null;
x?.tolocalelowercase(); // the line is "never" got evaluated

// type guard

function padleft(padding: number | string) {
  if (typeof padding === 'number') {
    console.log('the type of padding here' + padding + ' is a number');
  } else {
    console.log('the type of padding here' + padding + ' is a string');
  }
}

// truthiness narrowing
function printall(str: string | string[] | null) {
  if (typeof str === 'object') {
    for (const s of str) {
      //              ^ object is possibly 'null'.
      console.log(s);
    }
  } else if (typeof str === 'string') {
    console.log(str);
  } else {
    // do nothing
  }
}

function _printall(str: string | string[] | null) {
  if (str && typeof str === 'object') {
    for (const s of str) {
      //              ^ no error
      console.log(s);
    }
  } else if (typeof str === 'string') {
    console.log(str);
  } else {
    // do nothing
  }
}

// conditional assignment/ non-null assertion operator (postfix !)

let _foo: string;
if (1) {
  _foo = '1';
}

console.log(_foo);
console.log(_foo as string);
console.log(_foo!);

// equality narrowing

function example(x: string | number, y: string) {
  if (x === y) {
    x.tolowercase();
    console.log(x);
    //          ^(parameter) x: string
  } else {
    console.log(x);
  }

  if (x == y) {
    x.tolowercase();
  }
}

// truthiness narrowing

function truthinessnarrowing(x: null | string) {
  if (x) {
    x;
  } else {
    x;
  }
}

// assignment
let assignment = Math.random() < 0.5 ? 10 : 'hello world!';
//  ^ = let assignment: string | number
assignment = 1;

console.log(assignment);
//          ^ = let assignment: number;
// this is called observed type is different with **declared type**
// when the variable is be used  we only care about it observed type instead of declared type.

assignment = 'goodbye!';
// ^ = let assignment: number | string;
// this is called declared type is different with **observed type**
// when change the value of a variable, we only care about it declared type instead of observed type(the type of current value of the variable)

console.log(assignment);
//          ^ = let assignment: string

// type predicates

interface car {
  wheel: number;
}

type ship = {
  sprial: boolean;
};

const car: car = { wheel: 4 };
const ship: ship = { sprial: true };
const vehicle: car | ship = {
  wheel: 1,
};

function iscar(vehicle: car | ship): vehicle is car {
  if ((vehicle as car).wheel === undefined) return false;
  else return true;
}

if (iscar(vehicle)) {
  vehicle;
} else {
  vehicle;
}

(function () {
  // discriminated unions

  (function () {
    interface kinds {
      kind: 'kind1' | 'kind2';
      name1?: string;
      name2?: string;
    }

    function getname(foo: kinds) {
      if (foo.kind === 'kind1') {
        // will get foo.name1 may be undefined, but i know the name1 is exist
        // on the kind1.
        // can solve this by using the non-null type assertions `!`.
        // but the assertions are error prone
        return foo.name1.touppercase;
      }
    }
  });
});

//solution:
interface kind1 {
  kind: 'kind1';
  name1: string;
}

interface kind2 {
  kind: 'kind2';
  name2: string;
}

type kinds = kind1 | kind2;

function _getname(foo: kinds) {
  return foo.name1.touppercase();
}

function getname(foo: kinds) {
  if (foo.kind === 'kind1') {
    // the `kind` property is the discriminant property of kinds
    return foo.name1.touppercase;
  }
}

// more on functions
(function () {
  // function type expressions
  function greeter(fn: (sen: string) => string): void {
    console.log(fn('hello'));
  }

  function addname(name: string) {
    return 'hello ' + name;
  }

  greeter(addname);

  // using type alias
  type greetfunction = (sen: string) => string;
  function welcome(fn: greetfunction): void {
    console.log(fn('bilder'));
  }

  welcome(addname);

  let bar: (sen: string) => string;
  bar = (sen: string) => sen;
})();

(function () {
  //call signatures

  type foo = {
    description: string;
    (arg: number): boolean;
    new (arg: string): date;
  };

  function dosomething(fn: foo) {
    console.log(fn.description, fn(1));
    const date = new fn('1');
  }
})();
(function () {
  //construct signatures
  type foo = {
    description: string;
    new (): void;
    (): string;
  };

  function dosomething(fn: foo) {
    console.log(fn.description, new fn());
    fn();
  }

  function bar(con: new (name: string) => void) {
    return new con('birudo');
  }

  type con = {
    new (name: string): void;
  };

  const cons: con = function (name: string): void {};

  bar(cons);
})();

(function () {
  function firstelement(arr: any[]) {
    return arr[0];
  }

  const s = firstelement([1, 2, 3]);
  //    ^any

  function secondelement<type>(arr: type[]) {
    return arr[1];
  }

  const b = secondelement(['a', 's']);
  //    ^ string

  function foo<t>(arg: t): t {
    return res;
  }

  // inference
  function map<input, output>(
    arr: input[],
    fn: (arg: input) => output
  ): output[] {
    return arr.map(fn);
  }
  // constraints

  function hello<t>(name: t): t {
    return name;
  }

  const res = hello('hello');

  function longest<type>(a: type, b: type) {
    //                              ^ constrain the type must have property length
    if (a.length > b.length) {
      //   ^ プロパティ 'length' は型 'type' に存在しません
      return a;
    } else {
      return b;
    }
  }

  const obj1 = {
    length: 1,
  };
  const obj2 = {
    a: 1,
    length: 2,
  };

  longest(obj1, obj2);

  longest(1, 2);
  try {
    // working with constrained values
    function minimumlength<type extends { length: number }>(): type {
      return { length: 1 };
    } //         ^ 型 '{ length: number; }' を型 'type' に割り当てることはできません。
    //'              { length: number; }' は型 'type' の制約に代入できますが、'type' は制約 '{ length: number; }' の別のサブタイプでインスタンス化できることがあります。ts(2322)

    const arr = minimumlength([2, 3, 4], 9);
    console.log(arr.slice(1));
    //               ^crash here

    function foo<t extends any[]>(age: t): t {
      return age;
    }

    foo<number[] | string[]>([123, 11111111111]);
  } catch (err) {}
})();

try {
  // specifying type arguments
  function combine<type>(arr1: type[], arr2: type[]): type[] {
    return arr1.concat(arr2);
  }

  combine([1, 2], ['1', '1']);
  //                ^

  combine<string | number>([1, 2], ['1', '1']);
  //             ^ manually specify type
} catch (err) {}

//optional parameters
try {
  function f(n?: number) {
    n;
    // ^ number|undefined
  }
} catch (err) {}

try {
  // optional parameters in callback
  function myforeach(arr: any[], callback: (arg: any, index?: number) => void) {
    for (let i = 0; i < arr.length; i++) {
      callback(arr[i], i);
    }
  }

  myforeach([1, 2, 3], (a) => console.log(a));
  myforeach([1, 2, 3], (a, i) => {
    console.log(i.tofixed());
    //          ^
  });
} catch (err) {}

try {
  // optional modifiers
  function draw({ age: number }): number {
    return age;
    // ^名前 'age' が見つかりません。
  }
} catch (error) {}

try {
  // readonly
  interface foo {
    readonly cookie: string;
  }

  const bar: foo = {
    cookie: 'blue',
  };

  bar.cookie = 'red';
  //  ^ 読み取り専用プロパティであるため、'cookie' に代入することはできません。
} catch (error) {}

try {
  // function overloads
  function fn(x: number): void;
  function fn(x: string) {}

  function fn1(x: number): void;
  function fn1(x: string | number): void {}

  fn1('hello');

  function fn2(x: number): void;
  function fn2(): string {}

  function foo(str: string): string;
  function foo(num1: number, num2: number): number;
  function foo(strornum1: string | number, num2?: number): string | number {
    return 123;
  }
} catch (error) {}

try {
  // function generic
  function foo<type extends any[]>(arr: type) {
    return arr[1];
  }

  const res1 = foo([1, 1, 1]);

  function bar<type>(arr: type[]) {
    return arr[1];
  }

  const res2 = bar([1, 1, 1]);
} catch (error) {}

try {
  // declaring `this` in function
  // auto infer this
  const user = {
    id: 123,

    admin: false,
    becomeadmin: function () {
      this.admin = true;
    },
  };

  // explicitly set this pointer
  interface user {
    name: string;
  }

  interface cat {
    weight: number;
    foo: () => void;
  }

  const littlecat: cat = {
    weight: 1,
    foo: function () {},
  };

  function userfun(this: user): string {
    console.log('userfun');
    return 'userfun';
  }

  // littlecat.foo = foo;
  littlecat.foo = userfun;
  const bar = littlecat.foo();
  console.log(bar);
} catch (error) {}

try {
  interface person {
    age: number;
    say: () => void;
  }

  const person: person = {
    age: 1,
    say: function (): string {
      return '1';
    },
  };
} catch (error) {}

// try {

//   const foo: 'str1'|'str2';

//   const xy = [8, 5];
//   math.atan2(...xy);

//   const xy1: [8,5] = [8, 5]
//   math.atan2(...xy1)

//   const xy2 = [8, 5] as const;
//   math.atan2(...xy2){

// } catch (error)

// }

try {
  const foo: any = 1;
  const bar = foo as number;
  const ho = <number>foo;
} catch (error) {}

try {
  let foo1: 'str1' | 1 | 2 | true | false;

  let bar: 1 | 0;
  bar = number(true);
  //^型 'number' を型 '0 | 1' に割り当てることはできません。
} catch (error) {}

try {
  // object destructuring
  const { a, b } = {
    b: 1,
    a: 2,
  };

  console.log(a, b);
} catch (error) {}

try {
  type numberfun = () => number;
  type voidfun = () => void;
  const fun1: voidfun = function (): string {
    return 'fun1';
  }; // non-void function is assignable to void function type;
  const res1 = fun1();
  //^ res1: void;
  console.log(res1);
  console.log(res1.length);

  // const fun2:numberfun = function():void{}
  [1, 2, 3].foreach;
} catch (error) {}

try {
  interface foo {
    readonly bar: {
      height: number;
    };
  }

  let foo: foo = {
    bar: {
      height: 123,
    },
  };

  foo.bar.height = 1;
} catch (error) {}
try {
  // readonly modifier
  interface person {
    age: number;
  }
  interface readonlyperson {
    readonly age: number;
  }

  const person: person = {
    age: 10,
  };

  const readonlyperson: readonlyperson = person;
  const readonlyperson2 = person as readonlyperson;

  person.age = 11;

  console.log(readonlyperson);
} catch (error) {}

try {
  // index signatures
  interface numberindexobject {
    [index: number]: number;
  }

  interface iamanobject {
    [stringindex: string]: string;
    [numberkey: number]: number;
  }

  const foo: numberindexobject = {};
  foo[0] = 1;
  foo['123'] = true;
  foo[0] = '1';

  const bar: stringindexobject = {};
  bar[1] = 1;
  bar[0] = '0';
  bar['1'] = 0;

  console.log(foo);
} catch (error) {}

try {
  const obj = {
    tostring() {
      console.log('tostring called');
      return 'hello';
    },
  };

  const foo = {
    hello: 'hi',
  };
  console.log(foo[obj]); // hi
} catch (error) {}
try {
  const obj = new object();
  const foo: any = {};
  foo[obj] = 'what is the my key?';
  //   ^ 型 'object' はインデックスの型として使用できません。

  console.log(object.getownpropertynames(foo));
} catch (error) {}

try {
  const obj: { [username: string]: string } = {
    bilder: 'bilder',
  };

  interface person<t> {
    [name: string]: t;
    foo: string;
    //^型 'string' のプロパティ 'foo' を文字列インデックス型 't' に割り当てることはできません。ts(2411)
  }

  const foo: person<string> = {
    bilder: 'hello',
  };

  console.log(foo);
} catch (error) {}

try {
  /** okay */
  interface foo {
    x: number;
    y: number;
  }
  /** error */
  interface bar {
    x: number;
    y: string; // error: property `y` must be of type number
  }
} catch (error) {}

try {
  type index = 'birudo' | 'bilder' | 'loong';
  type fromindex = {
    [k in index]?: number;
  };

  const person: fromindex = {
    bilder: 1,
    birudo: 2,
  };

  type fromsomeindex<k extends string> = { [key in k]?: number };
  const foo: fromsomeindex<'a' | 'b' | 'c'> = {
    a: 1,
    b: 2,
    c: 3,
  };

  console.log(foo);
} catch (error) {}

try {
  interface colorful {
    color: string;
  }

  interface circle {
    radius: number;
  }

  interface colorfulcircle extends colorful, circle {}
  type colorfulcircle2 = colorful & circle;

  const cc: colorfulcircle = {
    color: 'red',
    radius: 42,
  };

  const cc1: colorfulcircle2 = {};
} catch (error) {}

try {
  let name: {
    first: string;
    second: string;
  } = {
    first: 'john',
    second: 'doe',
  };

  name = {
    // error : `second` is missing
    first: 'john',
  };
  name = {
    // error : `second` is the wrong type
    first: 'john',
    second: 1337,
  };
} catch (error) {}

try {
} catch (error) {}

try {
  foo = 123;
} catch (error) {}
try {
} catch (error) {}

try {
  const foo = 123;
  foo.tostring();

  document;
  window.screenleft;
  window.hello('123');
  process;

  buffer.from('123');
} catch (error) {}

try {
  // the array type
  let foo: array<number>;
  let bar: array<string>;
} catch (error) {}

try {
  // the readonlyarray type
  let readonlyarray: readonlyarray<number>;
  readonlyarray = [1, 2];
  readonlyarray[1] = 333;
  // ^ 型 'readonly number[]' のインデックス シグネチャは、読み取りのみを許可します。ts(2542)
} catch (error) {}
try {
  let foo: [string, number, number?] = ['1', 2, 3];
  foo.length; // (property) length: 2 | 3
  foo[0] = '2';
  foo = ['1', 2, 3];

  foo = [1, 2];
} catch (error) {}

try {
  let foo: readonly [string, number, number?] = ['1', 2, 3];
  foo.length; // (property) length: 2 | 3
  foo[0] = '2';
  foo = ['1', 2, 3];

  foo = [1, 2];
} catch (error) {}

try {
  // generic types
  function foo<t>(arg: t): t {
    return arg;
  }

  let bar: <t>(arg: t) => t;
  bar = function <t>(arg: t): t {
    return arg;
  };

  const fun1 = function <t>(arg: t): t {
    return arg;
  };
  const fun2 = foo;
} catch (error) {}

try {
  // generic type
  {
    interface genericfun {
      <t>(arg: t): t;
    }

    const genericfun: genericfun = function <t>(arg: t): t {
      return arg;
    };
  }

  {
    interface genericfun<t> {
      (arg: t): t;
    }

    let genericfun: genericfun<number> = function <t>(arg: t): t {
      return arg;
    };

    genericfun;

    genericfun = function (arg: number): number {
      return arg;
    };

    genericfun;
  }
} catch (error) {}

try {
  // the keyof type operator

  type point = {
    x: number;
    y: number;
  };

  type p = keyof point;
  let foo: p;
} catch (error) {}

try {
  let s: string;
  let n: typeof s;
  //  ^ let n: string

  console.log(n);
} catch (error) {}

try {
  // generic types

  function identity<t>(input: t): t {
    return input;
  }

  const anotheridentity: <u>(arg: u) => u = identity;
  type type1 = <t>() => t;
  type type2 = <u>() => u;

  type type3 = type2;
  // using type parameters in generic constraints
  // function getproperty<t, u extends keyof t>(obj: t, key: string) {
  // function getproperty<t, u extends keyof t>(obj: t, key: u) {
  function getproperty<t, u extends t>(obj: t, key: u) {
    return obj[key];
  }

  const obj = { a: 1, b: 2 };
  getproperty(obj, 'a');
  getproperty(obj, 'm');
} catch (error) {}

try {
  // keyof
  interface foo {
    name: string;
    age: number;
  }

  type bar = {
    name: string;
    age: number;
  };

  let foo: string | number = 'adfasd';
  console.log(foo);
  foo = '123';

  typeof foo;
  foo = 'name';
  type foo1 = typeof foo;
  let bar: keyof bar = 'age';

  interface foo {
    [key: string]: number;
  }

  type fookeys = keyof foo;
  //   ^ type fookeys = string | number
} catch (error) {}

try {
  // indexed access types
  type person = {
    name: string;
    age: number;
  };

  type i1 = person['name'];
  //   ^ type i1 = string
  type iall = person[keyof person];

  const arr = ['name', 'age', 1, { bibi: 1 }];
  const _arr = <const>['name', 'age', 1, { bibi: 1 }];
  type _arr0 = typeof _arr[0];
  type _arr3 = typeof _arr[3];
  type _arrall = typeof _arr[number];
  type arr0 = typeof arr[0];
  type age = typeof arr[number]['bibi'];
  type arrall = typeof arr[number];

  const myarray = [
    { name: 'alice', age: 15 },
    { name: 'bob', age: 23 },
    { name: 'eve', age: 38 },
    'string',
  ];

  type person_ = typeof myarray[number];
} catch (error) {}
try {
  type foo = {
    [key: string]: boolean | number;
    [index: number]: boolean;
  };

  type foostringkey = foo[string];
  // type foostringkey = number | boolean
} catch (error) {}
try {
  const arr: readonly any[] = [1, '1', true];
  type arrelementstype = typeof arr[number];
} catch (error) {}
try {
  const foo = <const>[1, '1', true];
  //const foo: readonly [1, '1', true];
  const bar = <const>{
    name: 'birudo',
    age: '21',
  };
  /*
const bar: {
  name: string;
  age: string;
};
*/

  /*
const bar: {
  readonly name: 'birudo';
  readonly age: '21';
};
*/
} catch (error) {}
try {
  // mapped types
  // type foo<t extends string | number | symbol> = {
  type foo<t> = {
    [key in keyof t]: string;
  };

  type didi = foo<1 | 2 | 3>;
  type dodo = 1 | 2 | 3;
  type dada = keyof '';
} catch (error) {}
try {
  type t = 'name' | 1;
  type foo = {
    // [key in t]: string;
    [key in t]: key;
  };

  type bar = {
    [key: string]: key;
  };
} catch (error) {}
try {
  type keys = 'stringkey' | 1 | 2;
  type foo1 = {
    [key in keys as 'index']: key;
  };

  type foo = {
    [key in keys as `_${key}_`]: `$${key}$`;
  };

  type foo3 = {
    [key in keys as key]: key;
  };
} catch (error) {}

try {
  type newkeytype = '1';
  type mappedtypewithnewproperties<type> = {
    [properties in keyof type as newkeytype]: type[properties];
  };
} catch (error) {}

try {
  type foo = 1 & string;
} catch (error) {}
try {
  // conditional type
  type bar = 'bar';
  type dada = 'dada';
  const numberorstring = math.random() ? 'hello' : 42;
  type numberorstring = typeof numberorstring;
  type barordada<t extends number | string> = t extends number ? bar : dada;
  type foo = barordada<numberorstring>;
} catch (q) {}
try {
  type foo = 'sdf' extends string ? boolean : bigint;
  type bar = 1 extends string ? boolean : bigint;
} catch (error) {}
try {
  type messageof<t> = t extends { message: unknown } ? t['message'] : never;
  type foo = messageof<{ message: string }>;
  type bar = messageof<{ bar: number }>;
} catch (error) {}
try {
  type distoarray<t> = t extends any ? t[] : never;
  type notdistoarray<t> = [t] extends [any] ? t[] : never;
  type toarray<t> = t[];
  type strnumarr = toarray<string | number>;
  type anotherstrnumarr = notdistoarray<string | number>;
  type strarrornumarr = distoarray<string | number>;
} catch (error) {}
try {
  type foo<t> = t extends { name: infer u; age: infer u } ? u : never;
  type t10 = foo<{ name: number; age: string }>;
  type t11 = foo<{ name: number; age: string }>;
} catch (error) {}
try {
  type foo<t> = t extends { a: infer u; b: (x: infer u) => void } ? u : never;
  type t10 = foo<{ a: string; b: (x: string) => void }>; // string
  type t12 = foo<{ a: string; b: (x: number) => void }>; // string
  type t11 = foo<{ a: string; b: number }>; // string | number
  // class implements
  interface foo {
    name: string;
  }

  // class bar implements foo {
  //   name = '1';
  // class Bar implements Foo {
  //   name = 'name';
  // }
} catch (error) {}

try {
  console.log(i);

  let i;
} catch (error) {}

try {
  const createResolvePromise = async (val: any, time: number = 1000) =>
    new Promise((resolve) => {
      setTimeout(() => {
        resolve(val);
      }, time);
    });

  const createRejectPromise = async (val: any, time: number = 1000) =>
    new Promise((_, reject) => {
      setTimeout(() => {
        reject(val);
      }, time);
    });

  const pro = createResolvePromise('0');
  // const pro_1 = createRejectPromise('1', 1000);
  // const pro_2 = createRejectPromise('2', 500);
  // const promiseAll = Promise.all([pro, pro_1, pro_2])
  const promiseAll = Promise.all([pro])
    .then((res) => {
      console.log(res);
    })
    .catch((reason) => {
      console.log('from catch');

      console.error(reason);
    });

  console.log(promiseAll);
} catch (error) {}
try {
  const createResolvePromise = async (val: any, time: number = 1000) =>
    new Promise((resolve) => {
      setTimeout(() => {
        resolve(val);
      }, time);
    });

  const createRejectPromise = async (val: any, time: number = 1000) =>
    new Promise((_, reject) => {
      setTimeout(() => {
        reject(val);
      }, time);
    });

  console.log('==================================================');
  // const pro = createResolvePromise('0');
  // const pro_1 = createRejectPromise('1', 1000);
  // const pro_2 = createRejectPromise('2', 500);
  // const promiseAll = Promise.all([pro, pro_1, pro_2])
  const p1 = Promise.all([1, 'a']);
  console.log(p1);

  setTimeout(() => {
    console.log('the stack is now empty');
    console.log(p1);
  });
} catch (error) {}
