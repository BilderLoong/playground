import { format } from 'path';

describe('test', () => {
  // Version 3
  it('test', () => {
    expect(replaceSpace(' 5 ')).toBe('%205%20');
  });

  function replaceSpace(s: string): string {
    const oldLen = s.length;
    const numberOfSpace = s.match(/\s/g)?.length ?? 0;
    const newLen = oldLen + numberOfSpace * 2;
    const arr = s.split('');
    arr.length = newLen;

    for (let j = newLen - 1, i = oldLen - 1; i < j; i--, j--) {
      if (arr[i] === ' ') {
        arr[j--] = '0';
        arr[j--] = '2';
        arr[j] = '%';
      } else {
        arr[j] = arr[i];
      }
    }

    return arr.join('');
  }
});

describe('test', () => {
  // Version 2
  it('test', () => {
    expect(replaceSpace(' 5 ')).toBe('%205%20');
  });

  function replaceSpace(s: string): string {
    const arr = [];
    for (const c of s) {
      if (c.match(/\s/)) {
        arr.push('%20');
      } else {
        arr.push(c);
      }
    }

    return arr.join('');
  }
});

describe('test', () => {
  // Version 1
  it('test', () => {
    expect(replaceSpace(' 5 ')).toBe('%205%20');
  });

  function replaceSpace(s: string): string {
    return s.replace(/ /g, '%20');
  }
});
