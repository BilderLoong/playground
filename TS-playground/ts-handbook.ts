export {};
try {
  const what = (BaseClass: Function) => {
    console.log(`what?`);
    return class extends BaseClass {
      constructor() {
        super();
        console.log("log from what dec");
      }
    };
  };

  @what
  class Foo {
    constructor() {
      console.log("Foo");
    }
  }

  console.log(123);
} catch (e) {}
try {
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
      console.log("Foo");
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
  const bar: baba = "1";
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

  const foo = new test("123");
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
    name: string = "base";
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
    x = "1";
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

  let obj1: object | null = { a: "1" };
  const wMap = new WeakMap([[obj1, "didi"]]);
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
  const stack = ["1", 1];
  let stacktop: string;
  stacktop = stack[0];
  stacktop = stack[0] as string;

  const myinputs = document.getelementsbyname(
    "input",
  ) as nodelistof<htmlinputelement>;

  // the below two type assertions have the same effect
  const myinput = document.getelementbyid("myinput") as htmlinputelement;
  const $myinput = <htmlinputelement>document.getelementbyid("myinput"); // can't be use in jsx

  const welcome = "hello" as any as number;
  const $welcome = "hello" as unknown as number;
  const _welcome = "hello" as number;
});

//----------------------------------------------------------------

(function () {
  // literal types

  let changingstring = "hello";
  changingstring = "welcome";

  const conststring = "hello";

  let foo: "bar";
  foo = "bar";
  foo = "hello";

  let myname: "birudo" | "bilder" | "cokoryuu";
  myname = "birudo";
  myname = "bilder";
  myname = "blider";

  // the below two is equal
  let booleanliteraltype: boolean;
  let _booleanliteraltype: true | false;

  // combine literal with non-literal
  let literaltypewithnonliteraltype: person | "bar";
  literaltypewithnonliteraltype = { name: "huzils", age: 21 };
  literaltypewithnonliteraltype = "bar";
  literaltypewithnonliteraltype = "foo";

  // literal inference
  const handlerequest = (url: string, method: "get" | "post") => {
    console.log(`${url},${method}`);
  };

  const req1 = { url: "url", method: "get" };

  /* will get error, because the inference type of req.method is
 string type don't correspond to the function argument
 type(a union type) */
  handlerequest(req1.url, req1.method);

  // workaround 1
  handlerequest(req1.url, req1.method as "get");

  // workaround 2
  const req2 = { url: "url", method: "get" as "get" };
  handlerequest(req2.url, req2.method);

  // workaround 3
  const req3 = {
    url: "url",
    method: "get" as "get",
  } as const;
  /*     ^ ensuring that all properties are assigned the literal type
  instead of a more general version like string or number */

  handlerequest(req3.url, req3.method);
});
// strictnullchecks on

function dosomething(x: string | null | undefined) {
  if (typeof x === "string") {
    x.tolowercase(); //narrowing
  }
  x?.touppercase(); // non-null assertion operator

  x.touppercase();
}

function _dosomething(x: string | null | undefined) {
  if (typeof x === "string") {
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
  if (typeof padding === "number") {
    console.log("the type of padding here" + padding + " is a number");
  } else {
    console.log("the type of padding here" + padding + " is a string");
  }
}

// truthiness narrowing
function printall(str: string | string[] | null) {
  if (typeof str === "object") {
    for (const s of str) {
      //              ^ object is possibly 'null'.
      console.log(s);
    }
  } else if (typeof str === "string") {
    console.log(str);
  } else {
    // do nothing
  }
}

function _printall(str: string | string[] | null) {
  if (str && typeof str === "object") {
    for (const s of str) {
      //              ^ no error
      console.log(s);
    }
  } else if (typeof str === "string") {
    console.log(str);
  } else {
    // do nothing
  }
}

// conditional assignment/ non-null assertion operator (postfix !)

let _foo: string;
if (1) {
  _foo = "1";
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
let assignment = Mat;
