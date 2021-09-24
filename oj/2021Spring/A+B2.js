const readline = require('readline');
const rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout,
});

const lines = [];
rl.on('line', (line) => {
  lines.push(line);
  const nLines = +lines[0]; // the number of lines

  // current group data all input
  if (lines.length == nLines + 1) {
    // remove the first line which contain the number of line
    const data = lines.slice(1); // contain the string of every line
    for (let i of data) {
      const [a, b] = i.split(' ').map((e) => parseInt(e));
      console.log(a + b);
    }
  }
});