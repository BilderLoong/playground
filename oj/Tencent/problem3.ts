import { Readable } from 'stream';
import { createInterface } from 'readline';

const DEBUG_STRING = '4\n 1 2 3\n 4 2 3\n 108 2 3    \n 108 1 40000\n';
let s;

if (DEBUG_STRING) {
  s = new Readable();
  s.push(DEBUG_STRING);
  s.push(null);
}

const inputStream = s || process.stdin;

const rl = createInterface({
  input: inputStream,
  out: process.stdout,
});

const lines: string[] = [];
rl.on('line', (line: string) => {
  lines.push(line);
  const lineNum = +lines[0];

  if (lines.length === lineNum + 1) {
    const data = lines.slice(1).map((l) => {
      return l
        .trim()
        .split(' ')
        .map((e) => parseInt(e));
    });

    data.forEach((l) => {
      const [n, x, y] = l;
      let curX = x;
      let numX = 0;

      let curY = y;
      let numY = 0;

      let curNum = 0;
      let res = -Infinity;

      while (curNum < n) {
        if (curX < curY) {
          numX++;
          res = curX = curX * numX;
        } else {
          numY++;
          res = curY = curY * numY;
        }

        curNum++;
      }
      console.log(res);
    });
  }
});
