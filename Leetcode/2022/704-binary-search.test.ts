it('should ', () => {
  const arr = [1, 2, 3, 4, 6, 8, 9];
  expect(binarySearchRecursion(arr, 38, 0, arr.length - 1)).toBe(-1);
  expect(binarySearchRecursion(arr, 6, 0, arr.length - 1)).toBe(4);

  function binarySearchRecursion(
    nums: number[],
    target: number,
    start: number,
    end: number
  ): number | void {
    if (start > end) {
      return -1;
    }

    const mid = Math.floor((start + end) / 2);
    if (nums[mid] === target) {
      return mid;
    } else if (nums[mid] > target) {
      return binarySearchRecursion(nums, target, start, mid - 1);
    } else if (nums[mid] < target) {
      return binarySearchRecursion(nums, target, mid + 1, end);
    }
  }
});

it('should ', () => {
  const arr = [1, 2, 3, 4, 6, 8, 9];
  expect(binarySearchIteration(arr, 38)).toBe(-1);
  expect(binarySearchIteration(arr, 6)).toBe(4);

  function binarySearchIteration(nums: number[], target: number) {
    let [left, right] = [0, nums.length - 1];

    while (left <= right) {
      const midI = Math.floor((left + right) / 2);
      const midV = nums[midI];

      if (midV < target) {
        left = midI + 1;
      } else if (midV > target) {
        right = midI - 1;
      } else {
        return midI;
      }
    }

    return -1;
  }
});
