it('should ', () => {
  // TC: O(n); SC: O(N)
  // Two time traverse with hash map.
  expect(firstUniqChar('abaccdeff')).toBe('b');
  expect(firstUniqChar('')).toBe(' ');

  function firstUniqChar(s: string): string {}
});

it('should ', () => {
  // TC: O(n); SC: O(N)
  // Two time traverse with hash map but the second traverse on hash map instead of the string.
  expect(firstUniqChar('abaccdeff')).toBe('b');
  expect(firstUniqChar('')).toBe(' ');

  function firstUniqChar(s: string): string {
    const map = new Map();

    for (const [i, c] of Array.from(s).entries()) {
      if (map.has(c)) {
        map.set(c, -1);
      } else {
        map.set(c, i);
      }
    }

    const res: [string, number] = [' ', Infinity];
    map.forEach((v, k) => {
      if (v !== -1) {
        if (v < res[1]) {
          res[0] = k;
          res[1] = v;
        }
      }
    });

    return res[0];
  }
});

it('should ', () => {
  // TC: O(n); SC: O(N)
  // Two time traverse with hash map.
  expect(firstUniqChar('abaccdeff')).toBe('b');
  expect(firstUniqChar('')).toBe(' ');

  function firstUniqChar(s: string): string {
    const map = new Map();

    for (const c of s) {
      if (map.has(c)) {
        map.set(c, false);
      } else {
        map.set(c, true);
      }
    }

    for (const c of s) {
      if (map.get(c)) {
        return c;
      }
    }

    return ' ';
  }
});
