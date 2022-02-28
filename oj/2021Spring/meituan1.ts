import readline = require('readline');

const rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout,
});

interface item {
  a: number;
  b: number;
}
const lines: string[] = [];
const news: item[] = [];

rl.on('line', (line: string) => {
  lines.push(line);
  const nLines = parseInt(lines[0]);

  if (lines.length === nLines + 1) {
    const data = lines.slice(1);

    for (line of data) {
      const current: string[] = line.split(' ');
      if (current[0] === 'query') {
        if (!news.length) console.log('null');
        else logItem(news.slice(0, 11));
      }

      if (current[0] === 'append') {
        const [a, b] = current.slice(1).map((e) => parseInt(e));
        let indexOfA = -1;
        for (let i = 0; i < news.length; i++) {
          if (news[i].a === a) {
            indexOfA = i;
            break;
          }
        }

        if (indexOfA === -1) {
          news.push({ a, b });
          sortItems(news);
        } else {
          news[indexOfA].b += b;
          sortItems(news);
        }
      }
    }
  }
});

function sortItems(items: item[]) {
  items.sort((m, n) => {
    if (m.b === n.b) return m.a - n.a;
    return n.b - m.b;
  });
}
// const items = [
//   { a: 2, b: 2 },
//   { a: 5, b: 5 },
//   { a: 4, b: 5 },
//   { a: 9, b: 9 },
//   { a: 1, b: 1 },
// ];
// sortItems(items);
// logItem(items);

// log the element to the console
function logItem(arr: item[]) {
  const res: number[] = [];
  for (let i = 0; i < 10; i++) {
    if (arr[i]) {
      res.push(arr[i].a);
    } else break;
  }

  console.log(res.join(' '));
}
