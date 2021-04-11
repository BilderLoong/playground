var anyType;
var aNumber = 0;
//         ^ this is called type assignment
var aNumberArr = [1, 2, 3];
//       ^  let aNumberArr: number[]
var aNumberOrStringArr = 1; // union type
function aFunctionWithStringReturn(name) {
    console.log("Hello," + name);
    return "Hello," + name;
}
[1, 2, 3].map(function (e, i) { return e.toString() + i.toString(); }); // contextual typing
// Type Assertions
var stack = ['1', 1];
var stackTop;
stackTop = stack[0];
stackTop = stack[0];
var myInputs = document.getElementsByName('input');
// The below two type assertions have the same effect
var myInput = document.getElementById('myInput');
var $myInput = document.getElementById('myInput'); // Can't be use in JSX
var welcome = 'hello';
var $welcome = 'hello';
var _welcome = 'hello';
//----------------------------------------------------------------
// Literal Types
var changingString = 'hello';
changingString = 'welcome';
var constString = 'hello';
var foo;
foo = 'bar';
foo = 'hello';
var myName;
myName = 'birudo';
myName = 'bilder';
myName = 'blider';
// the below two is equal
var booleanLiteralType;
var _booleanLiteralType;
// combine literal with non-literal
var literalTypeWithNonLiteralType;
literalTypeWithNonLiteralType = { name: 'huzils', age: 21 };
literalTypeWithNonLiteralType = 'bar';
literalTypeWithNonLiteralType = 'foo';
// Literal Inference
var handleRequest = function (url, method) {
    console.log(url + "," + method);
};
var req1 = { url: 'url', method: 'GET' };
/* Will get error, because the inference type of req.method is
 string type don't correspond to the function argument
 type(a union type) */
handleRequest(req1.url, req1.method);
// workaround 1
handleRequest(req1.url, req1.method);
// workaround 2
var req2 = { url: 'url', method: 'GET' };
handleRequest(req2.url, req2.method);
// workaround 3
var req3 = {
    url: 'url',
    method: 'GET'
};
/*     ^ ensuring that all properties are assigned the literal type
  instead of a more general version like string or number */
handleRequest(req3.url, req3.method);
// strictNullChecks on
function doSomething(x) {
    if (typeof x === 'string') {
        x.toLowerCase(); //narrowing
    }
    x === null || x === void 0 ? void 0 : x.toUpperCase(); // Non-null assertion operator
    x.toUpperCase();
}
function _doSomething(x) {
    if (typeof x === 'string') {
        return x.toLowerCase(); // narrowing
    }
    x === null || x === void 0 ? void 0 : x.toUpperCase(); // this line is never got evaluated
    x.toUpperCase();
}
// never type
var x = null;
x === null || x === void 0 ? void 0 : x.toLocaleLowerCase(); // The line is "never" got evaluated
// type guard
function padLeft(padding) {
    if (typeof padding === 'number') {
        console.log('the type of padding here' + padding + ' is a number');
    }
    else {
        console.log('the type of padding here' + padding + ' is a string');
    }
}
// Truthiness narrowing
function printAll(str) {
    if (typeof str === 'object') {
        for (var _i = 0, str_1 = str; _i < str_1.length; _i++) {
            var s = str_1[_i];
            //              ^ Object is possibly 'null'.
            console.log(s);
        }
    }
    else if (typeof str === 'string') {
        console.log(str);
    }
    else {
        // do nothing
    }
}
function _printAll(str) {
    if (str && typeof str === 'object') {
        for (var _i = 0, str_2 = str; _i < str_2.length; _i++) {
            var s = str_2[_i];
            //              ^ no error
            console.log(s);
        }
    }
    else if (typeof str === 'string') {
        console.log(str);
    }
    else {
        // do nothing
    }
}
// Conditional assignment/ non-null assertion operator (postfix !)
var _foo;
if (1) {
    _foo = '1';
}
console.log(_foo);
console.log(_foo);
console.log(_foo);
