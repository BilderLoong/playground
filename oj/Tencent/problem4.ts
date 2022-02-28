export {};

const test1 =
  'You say that you love rain,but you open your umbrella when it rains:rain';

const count = (str: string) => {
  const [toSearch, word] = str.split(':');
  const re = new RegExp(`${word}`, 'g');
  const res = toSearch.match(re);
  return res?.length;
};

console.log(count(test1));

const count2 = (str) => {
  const [toSearch, word] = str.split(':');
  let res = 0,
    i = 0;

  while (true) {
    const a = toSearch.indexOf(word, i);
    if (a !== -1) [res, i] = [res + 1, a + 1];
    else return res;
  }
};

console.log(count2(test1));
