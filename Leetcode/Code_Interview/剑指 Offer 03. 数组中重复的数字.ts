//--------------------question2-------------------------
(function () {
  const testArr = [2, 3, 5, 4, 3, 2, 6, 7];
  const res = findRepeatNumber(testArr);
  console.assert(res === 2 || res === 3, res);

  // time complexity:O(n^2)
  // space complexity:O(1)
  function findRepeatNumber(nums: number[]): number {
    for (let i = 0; i < nums.length; i++) {
      const e = nums[i];
      const index = nums.indexOf(e, i + 1); // O(n)
      if (index !== -1) return e;
    }
  }
})();
(function () {
  const testArr = [2, 3, 5, 4, 3, 2, 6, 7];
  const res = findRepeatNumber(testArr);
  console.assert(res === 2 || res === 3, res);

  // time complexity:O(ologn)
  // space complexity:O(1)
  function findRepeatNumber(nums: number[]): number {
    let end = nums.length - 1;
    let start = 1;
    while (end >= start) {
      const middle = Math.floor((end + start) / 2);
      // just need count once
      const count = countRange(nums, start, middle);

      if (end === start) return start;

      if (count > middle - start + 1) {
        end = middle;
      } else {
        start = middle + 1;
      }
    }
  }

  //---------------test----------------------
  const test2 = [1, 2, 3, 4, 2];
  const res2 = countRange(test2, 1, 2);
  console.assert(res2 === 3, res2);
  //-------------------------------------
  function countRange(arr: number[], l: number, r: number) {
    return arr.reduce((pre, cur) => {
      if (cur >= l && cur <= r) {
        return pre + 1;
      }
      return pre;
    }, 0);
  }
})();
(function () {
  // using an array store
  // space complexity: O(n)
  // time complexity: O(n)
  const testArr = [2, 3, 5, 4, 3, 2, 6, 7];
  const res = findRepeatNumber(testArr);
  console.assert(res === 2 || res === 3, res);

  function findRepeatNumber(nums: number[]): number {
    const helpArr: number[] = [];

    for (let i = 0; i < nums.length; i++) {
      const e = nums[i];
      if (!helpArr[e]) {
        helpArr[e] = e;
      } else {
        return e;
      }
    }
  }
})();
//--------------------question1-------------------------
(function () {
  // in place switch
  //-------------------test---------------------
  const testArr = [2, 3, 1, 0, 2, 5, 3];
  const res = findRepeatNumber(testArr);

  console.assert(res === 2 || res === 3, res);
  //---------------------------------------------
  function findRepeatNumber(nums: number[]): number {
    for (let i = 0; i < nums.length; i++) {
      const e = nums[i];
      while (e !== i) {
        if (e === nums[e]) return e;
        [nums[i], nums[e]] = [nums[e], nums[i]];
      }
    }
  }
})();
// using hash table
// time complexity O(n)
(function () {
  //-------------------test---------------------
  const testArr = [2, 3, 1, 0, 2, 5, 3];
  const res = findRepeatNumber(testArr);

  console.assert(res === 2 || res === 3, res);
  //---------------------------------------------
  function findRepeatNumber(nums: number[]): number {
    const hash = Object.create(null);
    for (let i = 0; i < nums.length; i++) {
      const e = nums[i];
      if (hash[e]) {
        return e;
      } else {
        hash[e] = true;
      }
    }
    return -1;
  }
})();

(function () {
  //using set instead of hash map
  //-------------------test---------------------
  const testArr = [2, 3, 1, 0, 2, 5, 3];
  const res = findRepeatNumber(testArr);

  console.assert(res === 2 || res === 3, res);
  //---------------------------------------------
  function findRepeatNumber(nums: number[]): number {
    const set = new Set();
    for (let i = 0; i < nums.length; i++) {
      const e = nums[i];
      if (set.has(e)) {
        return e;
      } else {
        set.add(e);
      }
    }
    return -1;
  }
})();
