const readline = require('readline');
const fs = require('fs');
const path = require('path');
const fileStream = fs.createReadStream(
  path.join(__dirname, '../../oj', 'input.txt')
);
const rl = readline.createInterface({
  input: fileStream,
});

const lines: string[] = [];
rl.on('line', (line: string) => {
  lines.push(line);
  const nLines: number = +lines[0]; // the number of lines

  // current group data all input
  if (lines.length == nLines + 1) {
    // remove the first line which contain the number of line
    const data: string[] = lines.slice(1); // contain the string of every line

    // your code

    // ---------------------
    lines.length = 0; // empty the arr for next group
  }
});

let lines1: string[] = [];
let nums = 0;
rl.on('line', (line: string) => {
  if (!lines1.length) {
    nums = +line;
  } else {
    lines1.push(line);
    if (nums === lines1.length) {
      // your code ...

      nums = 0;
      lines1 = [];
    }
  }
});
