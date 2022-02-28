const input1 = [
  [1, 1, 1, 2],
  [2, 1, 2, 2],
  [3, 2, 3, 3],
];
const res = categoryOfMaxWarehouseArea(input1);
console.assert(res === 1, res);

export function categoryOfMaxWarehouseArea(
  grid: (number | boolean)[][]
): number {
  let max = -Infinity;
  let ans: number;

  for (let x = 0; x < grid.length; x++) {
    for (let y = 0; y < grid[0].length; y++) {
      const cur = grid[x][y];
      if (typeof cur === 'boolean') {
        continue;
      } else {
        const obj = traverse(grid, cur, x, y);
        if (obj.res > max) {
          max = obj.res;
          ans = obj.category;
        }
      }
    }
  }

  return ans!;
}

function traverse(
  grid: (number | boolean)[][],
  category: number,
  i: number,
  j: number
): { res: number; category: number } {
  if (
    i < 0 ||
    j < 0 ||
    i >= grid.length ||
    j >= grid[0].length ||
    grid[i][j] !== category
  ) {
    return { res: 0, category };
  }

  grid[i][j] = false;

  let area = 1;
  area += traverse(grid, category, i + 1, j).res;
  area += traverse(grid, category, i - 1, j).res;
  area += traverse(grid, category, i, j - 1).res;
  area += traverse(grid, category, i, j + 1).res;

  return { res: area, category };
}
