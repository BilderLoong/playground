export {};
const input = [
  [1, 4, 7, 11, 15],
  [2, 5, 8, 12, 19],
  [3, 6, 9, 16, 22],
  [10, 13, 14, 17, 24],
  [18, 21, 23, 26, 30],
];
const input2 = [];
const input3 = [[-5]];

console.assert(findNumberIn2DArray(input3, -5), 3);
console.assert(!findNumberIn2DArray(input2, 0), 4);
console.assert(findNumberIn2DArray(input, 5), 1);
console.assert(!findNumberIn2DArray(input, 20), 2);

function findNumberIn2DArray(matrix: number[][], target: number): boolean {
  if (!matrix.length) return false;

  let row = 0;
  let column = matrix[0].length - 1;
  let cur;

  while (row < matrix.length && column >= 0) {
    cur = matrix[row][column];

    if (cur === target) {
      return true;
    } else if (cur > target) {
      column--;
    } else {
      row++;
    }
  }

  return false;
}
