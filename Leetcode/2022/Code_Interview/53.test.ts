it('should ', () => {
  expect(search([5, 7, 7, 8, 8, 10], 8)).toBe(2);
  expect(search([5, 7, 7, 8, 8, 10], 6)).toBe(0);

  /**
   * https://leetcode-cn.com/problems/zai-pai-xu-shu-zu-zhong-cha-zhao-shu-zi-lcof/solution/mian-shi-ti-53-i-zai-pai-xu-shu-zu-zhong-cha-zha-5/
   */
  function search(nums: number[], target: number): number {
    const getLeftBoundary = (nums: number[], target: number): number => {
      let [i, j] = [0, nums.length - 1];

      while (i <= j) {
        const mIndex = Math.floor((i + j) / 2);

        if (nums[mIndex] < target) {
          i = mIndex + 1;
        } else {
          j = mIndex - 1;
        }
      }

      return j;
    };

    return getLeftBoundary(nums, target + 1) - getLeftBoundary(nums, target);
  }
});
it('', () => {
  expect(search([5, 7, 7, 8, 8, 10], 8)).toBe(2);
  expect(search([5, 7, 7, 8, 8, 10], 6)).toBe(0);

  /**
   * https://leetcode-cn.com/problems/zai-pai-xu-shu-zu-zhong-cha-zhao-shu-zi-lcof/solution/mian-shi-ti-53-i-zai-pai-xu-shu-zu-zhong-cha-zha-5/
   */
  function search(nums: number[], target: number): number {
    let [i, j] = [0, nums.length - 1];

    // Left boundary
    while (i <= j) {
      const mIndex = Math.floor((i + j) / 2);

      if (nums[mIndex] < target) {
        i = mIndex + 1;
      } else {
        j = mIndex - 1;
      }
    }

    // If can't find the target in the array.
    if (nums[i] !== target) return 0;

    // The reason assign `j` to `leftBoundary`:
    // Because the when the `i === j`,
    // the `nums[mIndex]` is also equal to `target` and
    // both should point the most left `target` (if the `nums` contain `target`).
    // And according to the above conditional statement, the `j` variable
    // will move one left which exactly put the `j` variable on the desired left boundary.
    const leftBoundary = j;

    // Right boundary
    j = nums.length - 1; // No need to reset `i` to 0.
    while (i <= j) {
      const mIndex = Math.floor((i + j) / 2);

      if (nums[mIndex] <= target) {
        i = mIndex + 1;
      } else if (nums[mIndex] > target) {
        j = mIndex - 1;
      }
    }

    const right = i;

    return right - leftBoundary - 1;
  }
});

it('should ', () => {
  expect(search([5, 7, 7, 8, 8, 10], 6)).toBe(0);
  expect(search([5, 7, 7, 8, 8, 10], 8)).toBe(2);

  /**
   * @param nums
   * @param target
   * https://leetcode-cn.com/problems/zai-pai-xu-shu-zu-zhong-cha-zhao-shu-zi-lcof/solution/mian-shi-ti-53-i-zai-pai-xu-shu-zu-zhong-cha-zha-5/
   */
  function search(nums: number[], target: number): number {
    let [i, j] = [0, nums.length - 1];

    // Left boundary
    while (i <= j) {
      const mIndex = Math.floor((i + j) / 2);

      if (nums[mIndex] < target) {
        i = mIndex + 1;
      } else {
        j = mIndex - 1;
      }
    }
    const left = j;

    // Right boundary
    [i, j] = [0, nums.length - 1];
    while (i <= j) {
      const mIndex = Math.floor((i + j) / 2);

      if (nums[mIndex] <= target) {
        i = mIndex + 1;
      } else if (nums[mIndex] > target) {
        j = mIndex - 1;
      }
    }

    const right = i;

    return right - left - 1;
  }
});
