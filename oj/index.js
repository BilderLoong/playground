const readline = require('readline');
const fs = require('fs');
const path = require('path');
const fileStream = fs.createReadStream(path.join(__dirname, 'input.txt'));
const rl = readline.createInterface({
  input: fileStream,
});

let lines1 = [];
let nums = 0;
rl.on('line', (line) => {
  if (!nums) {
    nums = +line;
  } else {
    lines1.push(line);
    if (nums === lines1.length) {
      // your code ...
      for (let i of lines1) {
        console.log(
          i
            .split(' ')
            .map((e) => parseInt(e))
            .slice(1)
            .reduce((c, p) => c + p)
        );
      }
      //----------------
      nums = 0;
      lines1 = [];
    }
  }
});
