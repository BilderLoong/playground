it('test1', () => {
  // expect(hammingWeight((128).toString(2))).toBe(1);
  // expect(hammingWeight(00000000000000000000000000001011)).toBe(3);
});

/**
 * @param {number} a - a positive integer
 * @return {number}
 */
var hammingWeight = function (n: number): number {
  let i = 0;
  while (n) {
    n = n & (n - 1);
    i++;
  }

  return i;
};
