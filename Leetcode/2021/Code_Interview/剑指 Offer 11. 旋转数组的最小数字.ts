export {};

function minArray(numbers: number[]): number {
  let [left, right] = [0, numbers.length - 1];

  while (left < right) {
    const mid = Math.floor((right + left) / 2);

    /* after all the assignment, need make sure the 
    the minium value is still in the [left, right] except the line *
    */

    if (numbers[mid] < numbers[right]) {
      right = mid;
    } else if (numbers[mid] > numbers[right]) {
      left = mid + 1;
    } else if (numbers[mid] === numbers[right]) {
      return Math.min(...numbers.slice(left, right));
      // or
      //    right--; // *
    }
  }

  return numbers[left];
}

const test4 = [1, 3, 3];
const res4 = minArray(test4);
console.assert(res4 === 1, res4);

const test3 = [3, 1, 3];
const res3 = minArray(test3);
console.assert(res3 === 1, res3);

const test1 = [3, 4, 5, 1, 2];
const res1 = minArray(test1);
console.assert(res1 === 1, res1);

const test2 = [2, 2, 2, 0, 1];
const res2 = minArray(test2);
console.assert(res2 === 0, res2);
