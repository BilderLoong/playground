export function numberOfShelves(N: number): number[] {
  const matrix: number[][] = [];
  for (let i = 0; i < N; i++) {
    matrix.push([]);
  }

  let index = 1;
  let i = 0;
  let j = 0;

  for (let time = 0; time <= Math.floor(matrix.length / 2) - 1; time++) {
    // vertical
    while (j < matrix.length - time) {
      matrix[j++][i] = index++;
    }

    j--;
    while (j > time) {
      matrix[--j][++i] = index++;
    }

    // horizontal
    while (i > time + 1) {
      matrix[j][--i] = index++;
    }
  }

  let res: number[] = [];
  matrix.forEach((e) => {
    res = res.concat(e);
  });
  return res;
}

const res1 = numberOfShelves(3);
console.log(res1);
