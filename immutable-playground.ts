import { Map, Set } from 'immutable';

const map1 = Map({ a: 1 });
console.log(map1);

const set1 = Set([1]);
console.log(set1);

console.log(set1.has(1));
console.log(set1.has(2));

const newMap = map1.set('a', 50);
console.log(map1.get('a'));
console.log(newMap.get('a'));
