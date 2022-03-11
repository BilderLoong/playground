it('should ', () => {
  const twoDArray3 = [[]];
  expect(findNumberIn2DArray(twoDArray3, 1)).toBe(false);

  const twoDArray2 = [[1, 1]];
  expect(findNumberIn2DArray(twoDArray2, 0)).toBe(false);

  const twoDArray1 = [
    [1, 4, 7, 11, 15],
    [2, 5, 8, 12, 19],
    [3, 6, 9, 16, 22],
    [10, 13, 14, 17, 24],
    [18, 21, 23, 26, 30],
  ];

  expect(findNumberIn2DArray(twoDArray1, 5)).toBe(true);
  expect(findNumberIn2DArray(twoDArray1, 20)).toBe(false);

  function findNumberIn2DArray(matrix: number[][], target: number): boolean {
    if (!matrix.length) return false;

    let [row, column] = [0, matrix[0].length - 1];

    while (row < matrix.length && column >= 0) {
      const cur = matrix[row][column];
      if (cur === undefined) return false;
      if (cur > target) {
        column--;
      } else if (cur < target) {
        row++;
      } else if (cur === target) {
        return true;
      }
    }

    return false;
  }
});

// it('should ', () => {
//   const twoDArray1 = [
//     [1, 4, 7, 11, 15],
//     [2, 5, 8, 12, 19],
//     [3, 6, 9, 16, 22],
//     [10, 13, 14, 17, 24],
//     [18, 21, 23, 26, 30],
//   ];

//   expect(findNumberIn2DArray(twoDArray1, 5)).toBe(true);
//   expect(findNumberIn2DArray(twoDArray1, 20)).toBe(false);

//   function findNumberIn2DArray(matrix: number[][], target: number): boolean {
//     // Per line binary search.
//   }
// });
