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
      console.log(
        i
          .split(' ')
          .map((e) => parseInt(e))
          .slice(1)
          .reduce((c, p) => c + p)
      );
    }
    lines = [];
  }
});

// method 2
var lines1 = [];
var nums = 0;
rl.on('line', function (line) {
  if (!nums) {
    nums = +line;
  } else {
    lines1.push(line);
    if (nums === lines1.length) {
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
