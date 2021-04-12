const readline = require('readline');

const rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout,
});

rl.question('hello', (res) => {
  console.log('hi');
});

rl.on('line', (e) => {
  console.log(e);
});

