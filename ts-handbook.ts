export {}
  interface Person {
    name: string;
    age: number;
  }

  type FooType = {
    location: string[];
    middleName?: string; // optional properties
  };

  type bar = string | object; // union type and type aliases

  let anyType;
  let aNumber: number = 0;
  //         ^ this is called type assignment
  let aNumberArr = [1, 2, 3];
  //       ^  let aNumberArr: number[]

  let aNumberOrStringArr: number | string[] = 1; // union type

  function aFunctionWithStringReturn(name: string): string {
    console.log(`Hello,${name}`);
    return `Hello,${name}`;
  }

  [1, 2, 3].map((e, i) => e.toString() + i.toString()); // contextual typing
});

(function () {
  // Type Assertions
  const stack = ['1', 1];
  let stackTop: string;
  stackTop = stack[0];
  stackTop = stack[0] as string;

  const myInputs = document.getElementsByName(
    'input'
  ) as NodeListOf<HTMLInputElement>;

  // The below two type assertions have the same effect
  const myInput = document.getElementById('myInput') as HTMLInputElement;
  const $myInput = <HTMLInputElement>document.getElementById('myInput'); // Can't be use in JSX

  const welcome = ('hello' as any) as number;
  const $welcome = ('hello' as unknown) as number;
  const _welcome = 'hello' as number;
});

//----------------------------------------------------------------

(function () {
  // Literal Types

  let changingString = 'hello';
  changingString = 'welcome';

  const constString = 'hello';

  let foo: 'bar';
  foo = 'bar';
  foo = 'hello';

  let myName: 'birudo' | 'bilder' | 'cokoryuu';
  myName = 'birudo';
  myName = 'bilder';
  myName = 'blider';

  // the below two is equal
  let booleanLiteralType: boolean;
  let _booleanLiteralType: true | false;

  // combine literal with non-literal
  let literalTypeWithNonLiteralType: Person | 'bar';
  literalTypeWithNonLiteralType = { name: 'huzils', age: 21 };
  literalTypeWithNonLiteralType = 'bar';
  literalTypeWithNonLiteralType = 'foo';

  // Literal Inference
  const handleRequest = (url: string, method: 'GET' | 'POST') => {
    console.log(`${url},${method}`);
  };

  const req1 = { url: 'url', method: 'GET' };

  /* Will get error, because the inference type of req.method is
 string type don't correspond to the function argument
 type(a union type) */
  handleRequest(req1.url, req1.method);

  // workaround 1
  handleRequest(req1.url, req1.method as 'GET');

  // workaround 2
  const req2 = { url: 'url', method: 'GET' as 'GET' };
  handleRequest(req2.url, req2.method);

  // workaround 3
  const req3 = {
    url: 'url',
    method: 'GET' as 'GET',
  } as const;
  /*     ^ ensuring that all properties are assigned the literal type
  instead of a more general version like string or number */

  handleRequest(req3.url, req3.method);
});
// strictNullChecks on

function doSomething(x: string | null | undefined) {
  if (typeof x === 'string') {
    x.toLowerCase(); //narrowing
  }
  x?.toUpperCase(); // Non-null assertion operator

  x.toUpperCase();
}

function _doSomething(x: string | null | undefined) {
  if (typeof x === 'string') {
    return x.toLowerCase(); // narrowing
  }

  x?.toUpperCase(); // this line is never got evaluated
  x.toUpperCase();
}

// never type
let x: null = null;
x?.toLocaleLowerCase(); // The line is "never" got evaluated

// type guard

function padLeft(padding: number | string) {
  if (typeof padding === 'number') {
    console.log('the type of padding here' + padding + ' is a number');
  } else {
    console.log('the type of padding here' + padding + ' is a string');
  }
}

// Truthiness narrowing
function printAll(str: string | string[] | null) {
  if (typeof str === 'object') {
    for (const s of str) {
      //              ^ Object is possibly 'null'.
      console.log(s);
    }
  } else if (typeof str === 'string') {
    console.log(str);
  } else {
    // do nothing
  }
}

function _printAll(str: string | string[] | null) {
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

// Conditional assignment/ non-null assertion operator (postfix !)

let _foo: string;
if (1) {
  _foo = '1';
}

console.log(_foo);
console.log(_foo as string);
console.log(_foo!);

// Equality narrowing

function example(x: string | number, y: string) {
  if (x === y) {
    x.toLowerCase();
    console.log(x);
    //          ^(parameter) x: string
  } else {
    console.log(x);
  }

  if (x == y) {
    x.toLowerCase();
  }
}

// truthiness narrowing

function truthinessNarrowing(x: null | string) {
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

interface Car {
  wheel: number;
}

type Ship = {
  sprial: boolean;
};

const car: Car = { wheel: 4 };
const ship: Ship = { sprial: true };
const vehicle: Car | Ship = {
  wheel: 1,
};

function isCar(vehicle: Car | Ship): vehicle is Car {
  if ((vehicle as Car).wheel === undefined) return false;
  else return true;
}

if (isCar(vehicle)) {
  vehicle;
} else {
  vehicle;
}

(function () {
  // discriminated unions

  (function () {
    interface Kinds {
      kind: 'kind1' | 'kind2';
      name1?: string;
      name2?: string;
    }

    function getName(foo: Kinds) {
      if (foo.kind === 'kind1') {
        // will get foo.name1 may be undefined, but I know the name1 is exist
        // on the kind1.
        // Can solve this by using the non-null type assertions `!`.
        // but the assertions are error prone
        return foo.name1.toUpperCase;
      }
    }
  });
});

//solution:
interface Kind1 {
  kind: 'kind1';
  name1: string;
}

interface Kind2 {
  kind: 'kind2';
  name2: string;
}

type Kinds = Kind1 | Kind2;

function _getName(foo: Kinds) {
  return foo.name1.toUpperCase();
}

function getName(foo: Kinds) {
  if (foo.kind === 'kind1') {
    // the `kind` property is the discriminant property of Kinds
    return foo.name1.toUpperCase;
  }
}

// More on Functions
(function () {
  // Function Type Expressions
  function greeter(fn: (sen: string) => string): void {
    console.log(fn('hello'));
  }

  function addName(name: string) {
    return 'hello ' + name;
  }

  greeter(addName);

  // using type alias
  type GreetFunction = (sen: string) => string;
  function welcome(fn: GreetFunction): void {
    console.log(fn('bilder'));
  }

  welcome(addName);
  
  let bar: (sen: string) => string;
  bar = (sen:string)=>sen

})();

(function () {
  //Call signatures

  type foo = {
    description: string;
    (arg: number): boolean;
    new (arg:string): Date
  };

  function doSomething(fn: foo) {
  console.log(fn.description, fn(1));
  const date = new fn('1');
  }
})();
(function () {
  //Construct signatures
  type foo = {
    description: string;
    new (): void;
    (): string;
  };

  function doSomething(fn: foo) {
    console.log(fn.description, new fn());
    fn();
  }

  function bar(con: new (name:string)=>void){
    return new con('birudo');
  }

  type Con = {
    new (name:string):void 
  }

  const cons:Con =  function(name:string):void{

  }

  bar(cons)
})();

(function () {
function firstElement(arr: any[]) {
  return arr[0];
}

const s = firstElement([1, 2, 3]);
//    ^any

function secondElement<Type>(arr: Type[]) {
  return arr[1];
}

const b = secondElement(['a', 's']);
//    ^ string

function foo<T>(arg:T):T{
  return res
}

// inference
function map<Input, Output>(
  arr: Input[],
  fn: (arg: Input) => Output
): Output[] {
  return arr.map(fn);
}
// constraints

function hello<T>(name:T):T{
  return name
}

const res = hello("hello")

function longest<Type>(a: Type, b: Type) {
//                              ^ constrain the Type must have property length
if (a.length > b.length) {
  //   ^ プロパティ 'length' は型 'Type' に存在しません
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
function minimumLength<Type extends { length: number }>(
): Type {
return { length: 1 };
} //         ^ 型 '{ length: number; }' を型 'Type' に割り当てることはできません。
//'              { length: number; }' は型 'Type' の制約に代入できますが、'Type' は制約 '{ length: number; }' の別のサブタイプでインスタンス化できることがあります。ts(2322)

  const arr = minimumLength([2, 3, 4], 9);
  console.log(arr.slice(1));
  //               ^crash here

  function foo<T extends any[]>(age:T):T{
    return age;
  }

  foo<number[]| string[]>([123,11111111111])
} catch (err) {
  console.error(err);
}
})();
try {
  // specifying type arguments
  function combine<Type>(arr1: Type[], arr2: Type[]): Type[] {
    return arr1.concat(arr2);
  }

  combine([1, 2], ['1', '1']);
  //                ^

  combine<string | number>([1, 2], ['1', '1']);
  //             ^ manually specify Type
} catch (err) {}

//Optional parameters
try {
   function f(n?: number) {
    n;
    // ^ number|undefined
  }
} catch (err) {}

try {
  // Optional parameters in callback
  function myForEach(arr: any[], callback: (arg: any, index?: number) => void) {
    for (let i = 0; i < arr.length; i++) {
      callback(arr[i], i);
    }
  }

  myForEach([1, 2, 3], (a) => console.log(a));
  myForEach([1, 2, 3], (a, i) => {
    console.log(i.toFixed());
    //          ^
  });
} catch (err) {
  console.error(err);
}

try {
  // Optional Modifiers
function draw({age:number}):number{
  return age
       // ^名前 'age' が見つかりません。
}
} catch (error) {
  
}

try {
  // readonly
  interface Foo  {
    readonly cookie:string
  }

  const bar: Foo = {
    cookie:'blue'
  }

  bar.cookie = 'red'
  //  ^ 読み取り専用プロパティであるため、'cookie' に代入することはできません。
  
} catch (error) {
  
}

try {
  // Function overloads
  function fn(x:number):void
  function fn(x:string){
    
  }

function fn1(x:number):void;
function fn1(x:string | number):void{
}

fn1("hello");

  function fn2(x:number):void
  function fn2():string{
    
  }
  
function foo (str: string): string;
function foo (num1: number, num2: number): number;
function foo (strOrNum1: string | number, num2?: number): string | number {
  return 123
}

} catch (error) {
  
}

try {
 // Function generic 
  function foo<Type extends any[]>(arr:Type){
    return arr[1]
  }

  const res1 = foo([1,1,1]);

  function bar<Type>(arr:Type[]){
    return arr[1]
  }

  const res2 = bar([1,1,1]);

} catch (error) {
  
}

try {
  // Declaring `this` in function
  // auto infer this
const user = {
id: 123,

admin: false,
becomeAdmin: function () {
  this.admin = true;
},
}; 

// Explicitly set this pointer
interface User{
  name:string
}

interface Cat{
  weight:number;
  foo: ()=>void;
}

const littleCat:Cat = {
  weight:1,
  foo:function (){}
};

function userFun(this:User):string{
  console.log('userFun');
return 'userFun'
}

// littleCat.foo = foo;
littleCat.foo = userFun;
const bar = littleCat.foo()
console.log(bar);


} catch (error) {
  
}

try {
  interface Person{
    age:number;
    say:() => void
  }

  const person: Person = {
    age:1,
    say: function():string{
      return '1'
    }
  }
  
} catch (error) {
  
}

// try {

//   const foo: 'str1'|'str2';

//   const xy = [8, 5];
//   Math.atan2(...xy);

//   const xy1: [8,5] = [8, 5]
//   Math.atan2(...xy1)

//   const xy2 = [8, 5] as const;
//   Math.atan2(...xy2){

// } catch (error) 
  
// }


try {
  const foo:any = 1;
 const bar = foo as number ;
 const ho = <number>foo;

} catch (error) {
  
}

try {
  
 let foo1: "str1" | 1 | 2 | true | false ;

 let bar: 1 | 0;
 bar = Number(true);
 //^型 'number' を型 '0 | 1' に割り当てることはできません。

} catch (error) {
  
}

try {
  // Object Destructuring
  const {a,b} = {
    b :1,
    a:2
  }

  console.log(a,b);
  
} catch (error) {
  
}

try {
  type numberFun = ()=>number;
  type voidFun = ()=>void;
  const fun1:voidFun = function(): string{ return 'fun1'; } // non-void function is assignable to void function type;
  const res1 = fun1(); 
        //^ res1: void;
  console.log(res1);
  console.log(res1.length);
  
  // const fun2:numberFun = function():void{}
  [1,2,3].forEach
} catch (error) {
  
}

try {
interface Foo {
  readonly bar: {
    height:number;
  };
}

let foo: Foo = {
  bar: {
    height: 123
  },
}

foo.bar.height = 1;


} catch (error) {
  
}