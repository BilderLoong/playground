const readline = require('readline');
const rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout,
});

const lines = [];
rl.on('line', (line) => {
  const [a, b] = line.split(' ').map((e) => parseInt(e));
  if (a + b === 0) return;
  console.log(a + b);
});
