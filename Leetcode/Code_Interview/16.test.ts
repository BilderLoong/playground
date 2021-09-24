try {
  describe('first', () => {
    it('test1', () => {
      expect(myPow(2.0, 10)).toBe(1024.0);
    });

    it('test2', () => {
      expect(myPow(2.1, 3)).toBeCloseTo(9.261);
    });

    it('test3', () => {
      expect(myPow(2, -2)).toBe(0.25);
    });

    it('test4', () => {
      // expect(myPow(0.00001, 2147483647)).toBe(0.25);
    });

    it('test5', () => {
      // expect(myPow(0.00001, 2147483647)).toBe(0.25);
    });
  });

  function myPow(x: number, n: number): number {
    if (n === 0) return 1;
    if (n === 1) return x;

    if (n > 0) {
      return myPow(x, Math.floor(n / 2)) * myPow(x, Math.ceil(n / 2));
    }

    if (n < 0) {
      return 1 / (myPow(x, Math.floor(-n / 2)) * myPow(x, Math.ceil(-n / 2)));
    }
  }
} catch (error) {}

try {
  // O(n)
  describe('first', () => {
    it('test1', () => {
      expect(myPow(2.0, 10)).toBe(1024.0);
    });

    it('test2', () => {
      expect(myPow(2.1, 3)).toBeCloseTo(9.261);
    });

    it('test3', () => {
      expect(myPow(2, -2)).toBe(0.25);
    });
  });

  function myPow(x: number, n: number) {
    if (n === 0 && x !== 0) return 1;

    if (n > 0) {
      if (x === 0) return 0;
      if (x !== 0) {
        let res: number = x;
        for (let i = 1; i < n; i++) {
          res = x * res;
        }

        return res;
      }
    } else {
      if (x === 0) return Infinity;
      if (x !== 0) {
        let res: number = x;
        for (let i = 1; i < -n; i++) {
          res = x * res;
        }

        return 1 / res;
      }
    }
  }
} catch (error) {}
it('test1', () => {
  expect(myPow(2, 10)).toBe(1024);
});
function myPow(x: number, n: number): number {
  if (x === 0) return 0;

  let res = 1;
  if (n < 0) [x, n] = [1 / x, -n];

  while (n) {
    if (n & 1) {
      res *= x;
    }

    x *= x;
    n >>= 1;
  }

  return res;
}
