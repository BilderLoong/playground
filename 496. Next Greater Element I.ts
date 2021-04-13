// console.log(nextGreaterElement([2, 4], [1, 2, 3, 4]));

function nextGreaterElement(nums1: number[], nums2: number[]): number[] {
  const stack: number[] = [];
  const map = {};

  nums2.forEach((e ) => {
    while (stack.length && e > stack[stack.length - 1]) {
      map[stack.pop()] = e;
    }

    stack.push(e);
  });


  stack.forEach((e) => (map[e] = -1));
  return nums1.map((e) => map[e]);
}

function nextGreaterElement(nums1: number[], nums2: number[]): number[] {
  const res: number[] = [];

  nums1.forEach((e) => {
    let currentNGE: number | undefined;

    for (
      let i = nums2.indexOf(e);
      i < nums2.length && currentNGE === undefined;
      i++
    ) {
      if (nums2[i] > e) {
        currentNGE = nums2[i];
      }
    }

    res.push(currentNGE ?? -1);
  });

  return res;
}
