export {};
import readline = require('readline');

const rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout,
});
//------------------------------------

let num = 1; // current line number
let n: number, m: number, k: number;
let A = 0;
let B = 0;

rl.on('line', (line: string) => {
  if (num === 1) {
    [n, m, k] = getNumberFromLine(line);
    // console.log(n, m, k);
  } else if (num <= n + 1) {
    const [x, y] = getNumberFromLine(line);
    if (y >= k) A += x * y;
    // console.log(`Value of A:${A}`);
  } else {
    const [x, y] = getNumberFromLine(line);
    if (y >= k) B += x * y;
    // console.log(`Value of B:${B}`);
    if (num === m + n + 1) {
      console.log(`${A} ${B}`);
      console.log(A > B ? 'A' : 'B');
    }
  }

  num++;
});

function getNumberFromLine(line: string) {
  return line.split(' ').map((e) => parseInt(e));
}
