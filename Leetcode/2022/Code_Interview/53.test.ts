it('Binary search', () => {
  expect(missingNumber([0])).toBe(1);
  expect(missingNumber([1])).toBe(0);
  expect(missingNumber([0, 1, 3])).toBe(2);
  expect(missingNumber([0, 1, 2, 3, 4, 5, 6, 7, 9])).toBe(8);

  function missingNumber(nums: number[]): number {
    let [l, r] = [0, nums.length - 1];

    while (l <= r) {
      const mI = Math.floor((r + l) / 2);
      const mV = nums[mI];

      if (mI === mV) {
        l = mI + 1;
      } else {
        r = mI - 1;
      }
    }

    return l;
  }
});
it('Brutal force', () => {
  expect(missingNumber([1])).toBe(0);
  expect(missingNumber([0])).toBe(1);
  expect(missingNumber([0, 1])).toBe(2);
  expect(missingNumber([0, 1, 3])).toBe(2);
  expect(missingNumber([0, 1, 2, 3, 4, 5, 6, 7, 9])).toBe(8);

  function missingNumber(nums: number[]): number | void {
    for (let i = 0; i < nums.length; i++) {
      if (nums[i] !== i) {
        return i;
      }
    }

    // Which means the last element is missing in the array.
    return nums.length;
  }
});
