const Immutable = require('seamless-immutable');
const iArr1 = Immutable([1, 2, 3]);
const arr1 = [1, 2, 3];

console.log(Immutable.isImmutable(arr1));
console.log(Immutable.isImmutable(iArr1));

try {
  iArr1[1] = 3;
} catch (error) {
  console.error(error);
}
console.log(iArr1);

const imObj1 = Immutable({ foo: 1 });
console.log(imObj1.foo); // 1

const imObj2 = Immutable.set(imObj1, 'bar', 2);
console.log(imObj1.bar); // undefined
console.log(imObj2.bar); // 2

console.log(imObj2); // { foo: 1, bar: 2 }
const imObj3 = Immutable.merge(imObj2, { foo: 2, name: 'bilder' });
console.log(imObj3); // { foo: 2, bar: 2, name: 'bilder' }

const nestObj = Immutable.setIn(
  Immutable({}),
  ['nest1', 'nest2', 'nest3'],
  'end'
);
console.log(nestObj);
// What is difference between z
console.log(nestObj.nest1.nest2.nest3);
console.log(Immutable.getIn(nestObj, ['nest1', 'nest2', 'nest3']));

const empty = Immutable({});
try {
  empty.nest1 = 1;
} catch (error) {
  console.error(error);
}

const res1 = Immutable([1, 2, 3]).reduce(
  (pre: number, cur: number) => pre + cur
);
console.log(res1);

try {
  const res2 = Immutable([1, 2, 3]).reduce((pre: Set<number>, cur: number) => {
    pre.add(cur);
    // console.log(Immutable.isImmutable(pre));
    return pre;
  }, new Set());

  console.log({ res2 });
  console.log(Immutable.isImmutable(res2));
} catch (error) {
  console.error(error);
}
const res3 = Immutable(new Set());
console.log(res3);
