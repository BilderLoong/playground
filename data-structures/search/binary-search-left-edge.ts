export {};

(function () {
  const test1 = [1, 2, 2, 2, 3];
  const res1 = binaryLeft(test1, 2, 0, test1.length);
  console.assert(res1 === 1, res1);

  const test2 = [2, 2, 2, 2, 2];
  const res2 = binaryLeft(test2, 2, 0, test2.length);
  console.assert(res2 === 0, res2);

  const test3 = [1];
  const res3 = binaryLeft(test3, 2, 0, test3.length);
  console.assert(res3 === -1, res3);

  function binaryLeft(
    arr: number[],
    target: number,
    start: number,
    end: number
  ): number {
    if (start === end) return arr[start] === target ? start : -1;

    const mid = Math.floor((end - start) / 2);

    // every recursion binary search should
    // at least exclude one element (the middle element)
    // and on average should exclude half of all the element
    if (target === arr[mid]) {
      return binaryLeft(arr, target, start, mid);
    } else if (target > arr[mid]) {
      return binaryLeft(arr, target, mid + 1, end);
    } else if (target < arr[mid]) {
      /* 
        because right isn't in the search interval,
        so assign mid to the right exclude the middle element
      */
      return binaryLeft(arr, target, start, mid);
    }
  }
})();

(function () {
  // iteration
  const test1 = [1, 2, 2, 2, 3];
  const res1 = binaryLeft(test1, 2);
  console.assert(res1 === 1, res1);

  const test2 = [2, 2, 2, 2, 2];
  const res2 = binaryLeft(test2, 2);
  console.assert(res2 === 0, res2);

  const test3 = [1];
  const res3 = binaryLeft(test3, 2);
  console.assert(res3 === -1, res3);

  // return -1 if not found
  // return the index of the target
  function binaryLeft(arr: number[], target: number): number {
    let left = 0;
    let right = arr.length; // search target in [left, right)

    while (left < right) {
      //        ^ loop end when left === right / [left, left) / [right, right)
      const mid = Math.floor((right - left) / 2);

      // every loop should
      // at least exclude one element (the middle element)
      // and on average should exclude half of all the element
      if (target === arr[mid]) {
        right = mid; //
      } else if (target > arr[mid]) {
        // when there are only one element in the searching
        // span, after the below assignment the left will become to
        // the right (because when only element in the searching span
        // the mid is equal to left)
        left = mid + 1;
      } else if (target < arr[mid]) {
        /* 
        because right isn't in the search interval,
        so assign mid to the right exclude the middle element
      */
        right = mid;
      }
    }

    return arr[left] === target ? left : -1;
  }
})();
