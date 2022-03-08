it('Solution 1', () => {
  /*
   * In-place exchange
   * @description Use the inputs array as a map,
   *              whose keys are the index, value are the number.
   *
   * Time Complexity: O(n), Space Complexity: O(1)
   */
  const res3 = findRepeatNumber([0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 11]);
  expect([11]).toContain(res3);

  const res2 = findRepeatNumber([3, 4, 2, 0, 0, 1]);
  expect([0]).toContain(res2);

  const res = findRepeatNumber([2, 3, 1, 0, 2, 5, 3]);
  expect([2, 3]).toContain(res);

  function findRepeatNumber(nums: number[]): number | void {
    for (let i = 0; i < nums.length; ) {
      const num = nums[i];

      if (i === num) {
        // If the value locate in right place.
        i++; // Note that only increase `i` when the element is already checked.
        continue;
      } else if (nums[num] === num) {
        // If there is a same value already in the 'map'.
        return num;
      } else {
        // Store the value into 'map'.

        [nums[i], nums[num]] = [nums[num], nums[i]];
      }
    }
  }
});

it('Solution 2 ', () => {
  // Solution 1:
  // Time Complexity: O(n), Space Complexity: O(n)
  const res = findRepeatNumber([2, 3, 1, 0, 2, 5, 3]);
  expect([2, 3]).toContain(res);

  function findRepeatNumber(nums: number[]): number | void {
    const table = new Set<number>();

    for (let num of nums) {
      if (table.has(num)) {
        return num;
      } else {
        table.add(num);
      }
    }
  }
});
