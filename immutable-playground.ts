import Immutable from 'seamless-immutable';

const foo = Immutable({ name: 'birudo' });

Object.keys(foo).forEach((k) => {
  console.log(k, foo.getIn([ k ]);
});
