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

// Type Assertions
const myInputs = document.getElementsByName(
  'input'
) as NodeListOf<HTMLInputElement>;

// The below two type assertions have the same effect
const myInput = document.getElementById('myInput') as HTMLInputElement;
const $myInput = <HTMLInputElement>document.getElementById('myInput'); // Can't be use in JSX

const welcome = ('hello' as any) as number;
const $welcome = ('hello' as unknown) as number;
const _welcome = 'hello' as number;
//----------------------------------------------------------------

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
function printAll(strs: string | string[] | null) {
  if (typeof strs === 'object') {
    for (const s of strs) {
      //              ^ Object is possibly 'null'.
      console.log(s);
    }
  } else if (typeof strs === 'string') {
    console.log(strs);
  } else {
    // do nothing
  }
}

function _printAll(strs: string | string[] | null) {
  if (strs && typeof strs === 'object') {
    for (const s of strs) {
      //              ^ no error
      console.log(s);
    }
  } else if (typeof strs === 'string') {
    console.log(strs);
  } else {
    // do nothing
  }
}