const readline = require('readline');
const rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout,
});

const lines = [];
rl.on('line', (line) => {
  if (line === '0') return;

  const arr = line
    .split(' ')
    .slice(1)
    .map((e) => parseInt(e));

  const res = arr.reduce((p, c) => {
    return p + c;
  }, 0);

  console.log(res);
});
