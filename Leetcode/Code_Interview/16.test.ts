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
