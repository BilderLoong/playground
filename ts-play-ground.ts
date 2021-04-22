const proto = {
  location: 'on the proto',
};

const person2 = Object.create(proto);
Object.defineProperty(person2, 'location', {
  enumerable: false,
  value: 'on the instance',
});

// person2.location = 'on the instance';

for (const key in person2) {
  if (Object.prototype.hasOwnProperty.call(person2, key)) {
    console.log(key);
  }
}

for (const key of Object.getOwnPropertyNames(person2)) {
  console.log(`${key}: ${person2[key]}`);
}

// Object.key()
// const protoOfFoo = {
//   location: 'on the proto',
// };

// const foo = Object.create(protoOfFoo);

// Object.defineProperties(foo, {
//   one: { enumerable: true, value: 1 },
//   two: { enumerable: false, value: 2 },
// });

// console.log(Object.keys(foo));
// console.log(Object.getOwnPropertyNames(foo));

class Parent {
  parentPro = 'parent';
  foo() {}
}

class Child extends Parent {
  childPro = 'child';
  bar() {}
}

const child = new Child();
console.log(child);

console.log(child.hasOwnProperty('parentPro')); // true
console.log(child.hasOwnProperty('foo'));
console.log(child.hasOwnProperty('bar'))

function Parent2() {
  this.parentPro = 'parent';
}
Parent2.prototype.foo = function () {};

function Child2() {
  Parent2();
  this.childPro = 'child';
}
Child2.prototype = new Parent2();

const child2 = new Child();
console.log(child2);

;

console.log(child2.hasOwnProperty('parentPro')); // true
console.log(child2.hasOwnProperty('foo'));
console.log(child2.hasOwnProperty('bar'));
