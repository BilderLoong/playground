const input1 = '10001101';
const ans = 'PBGBBBGPPPBPGGG';
const res = bpg(input1);

console.assert(res === ans, res);

export function bpg(s: string): string {
  let res = '';
  traverse(s);
  return res;

  function traverse(s: string): string {
    if (s === '0') {
      res += 'B';
      return 'B';
    }
    if (s === '1') {
      res += 'P';
      return 'P';
    }

    const mid = Math.floor((s.length - 1) / 2);
    const leftSub = s.slice(0, mid + 1);
    const rightSub = s.slice(mid + 1, s.length);

    const left = traverse(leftSub);
    const right = traverse(rightSub);

    let cur: string;

    if (left !== right) cur = 'G';
    else cur = left;

    res += cur;

    return cur;
  }
}
