/**
 Do not return anything, modify nums1 in-place instead.
 */
function merge(nums1: number[], m: number, nums2: number[], n: number): void {
  let a = m + n - 1;
  m--;
  n--;
  while (m >= 0 && n>= 0) {
    if (nums1[m] <= nums2[n]) {
      nums1[a--] = nums2[n--];
    } else {
      nums1[a--] = nums1[m--];
    }
  }


  // Pay attention on the edge case
  if (n >= 0) {
    nums1.splice(0, n+1 , ...nums2.slice(0, n+1 ));
  }
}

let input1 = [2, 0];
let m = 1;
let input2 = [1];
let n = 1;

merge(input1, m, input2, n);
console.log(input1);

// var merge = function (nums1, m, nums2, n) {
//   let current = m + n - 1;
//   while (m > 0 && n > 0) {
//     nums1[current--] = nums1[m - 1] >= nums2[n - 1] ? nums1[--m] : nums2[--n];
//   }

//   if (n > 0) {
//     nums1.splice(0, n, ...nums2.slice(0, n));
//   }
// };
