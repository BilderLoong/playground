const readline = require('readline');

const rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout,
});

rl.on('line', (e) => {
  const [a, b] = e.split(' ').map((e) => parseInt(e));
  console.log(a + b);
});