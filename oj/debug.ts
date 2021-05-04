export = {};
import fs = require('fs');
import path = require('path');
import readline = require('readline');

const rl = readline.createInterface({
  input: fs.createReadStream(path.resolve(__dirname, 'input.txt')),
});

const arr=['./']
arr.fill('./a/b',arr.length-1,)

rl.on('line', (line: string) => {
  const arr = line.split('/').filter((e) => e !== '');
  const res = [];
  while (arr.length) {
    const top = arr.pop();
    if (top === '.') continue;
    else if (top === '..') {
      const index = arr.indexOf('..');
      if (index > 0) arr.splice(index - 1, 1);
      else {
        console.log(-1);
        return;
      }
    } else res.unshift(top);
  }

  console.log(`./${res.join('/')}`);
});

function getNumber(str: string) {
  return str.split(' ').map((e) => parseInt(e));
}
