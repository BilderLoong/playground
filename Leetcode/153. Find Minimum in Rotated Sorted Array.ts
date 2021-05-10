export {};

// The difference of the below two solution is just different style handle
// edge cases
(function () {
  const test1 = [3, 4, 5, 1, 2];
  const result1 = findMin(test1);
  console.assert(result1 === 1, result1);

  const test2 = [4, 5, 6, 7, 0, 1, 2];
  const result2 = findMin(test2);
  console.assert(result2 === 0, result2);
  //------------------------------------------------------------

  function findMin(nums: number[]): number {
    let [left, right] = [0, nums.length - 1];

    while (left <= right) {
      const mid = Math.floor((right + left) / 2);

      if (nums[mid] > nums[right]) { //
        left = mid + 1;
      } else if (nums[mid] <= nums[right]) {//
        right = mid;
      }
    }

    return nums[right];//
  }
})();

(function () {
  const test1 = [3, 4, 5, 1, 2];
  const result1 = findMin(test1);
  console.assert(result1 === 1, result1);

  const test2 = [4, 5, 6, 7, 0, 1, 2];
  const result2 = findMin(test2);
  console.assert(result2 === 0, result2);

  function findMin(nums: number[]): number {
    let [left, right] = [0, nums.length - 1];

    while (left <= right) {
      const mid = Math.floor((right + left) / 2);

      if (nums[mid] >= nums[right]) {//
        left = mid + 1;
      } else if (nums[mid] < nums[right]) { //
        right = mid;
      }
    }

    return nums[right]; //
  }
})();
(function () {
  const test1 = [3, 4, 5, 1, 2];
  const result1 = findMin(test1);
  console.assert(result1 === 1, result1);

  const test2 = [4, 5, 6, 7, 0, 1, 2];
  const result2 = findMin(test2);
  console.assert(result2 === 0, result2);

  function findMin(nums: number[]): number {
    let [left, right] = [0, nums.length - 1];

    while (left <= right) {
      const mid = Math.floor((right + left) / 2);

      if (nums[mid] > nums[right]) {
        left = mid + 1;
      } else if (nums[mid] < nums[right]) {
        right = mid;
      } else if (nums[mid] === nums[right]) {
        return Math.min(...nums.slice(left, mid + 1));
      }
    }

    return nums[left];
  }
})();
