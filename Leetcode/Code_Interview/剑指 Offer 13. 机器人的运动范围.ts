export {};
(function () {
  // DFS
  //using set instead of two dimension matrix to store visited information
  const res3 = movingCount(3, 2, 17);
  console.assert(res3 === 6, res3);

  const res1 = movingCount(2, 3, 1);
  console.assert(res1 === 3, res1);

  const res2 = movingCount(3, 1, 0);
  console.assert(res2 === 1, res2);

  function movingCount(m: number, n: number, k: number): number {
    const set: Set<string> = new Set();

    return dfs(0, 0, m, n, k, set);
  }

  function dfs(
    i: number,
    j: number,
    m: number,
    n: number,
    k: number,
    set: Set<string>
  ): number {
    const sum = getDigitalSum(i) + getDigitalSum(j);

    if (sum > k || i < 0 || i >= m || j < 0 || j >= n || set.has(`${i},${j}`))
      return 0;

    set.add(`${i},${j}`);

    const res =
      1 +
      dfs(i + 1, j, m, n, k, set) +
      dfs(i - 1, j, m, n, k, set) +
      dfs(i, j + 1, m, n, k, set) +
      dfs(i, j - 1, m, n, k, set);

    return res;
  }

  function getDigitalSum(n: number) {
    let sum = 0;
    while (n > 0) {
      sum += n % 10;
      n = Math.floor(n / 10);
    }

    return sum;
  }
})();
(function () {
  //DFS
  //using two dimension matrix to store visited information
  const res3 = movingCount(3, 2, 17);
  console.assert(res3 === 6, res3);

  const res1 = movingCount(2, 3, 1);
  console.assert(res1 === 3, res1);

  const res2 = movingCount(3, 1, 0);
  console.assert(res2 === 1, res2);

  function movingCount(m: number, n: number, k: number): number {
    const booleanMatrix = matrixMaker(m, n, false);

    return dfs(0, 0, m, n, k, booleanMatrix);
  }

  function dfs(
    i: number,
    j: number,
    m: number,
    n: number,
    k: number,
    booleanMatrix: boolean[][]
  ): number {
    const sum = getDigitalSum(i) + getDigitalSum(j);

    if (sum > k || i < 0 || i >= m || j < 0 || j >= n || booleanMatrix[i][j])
      return 0;

    booleanMatrix[i][j] = true;

    const res =
      1 +
      dfs(i + 1, j, m, n, k, booleanMatrix) +
      dfs(i - 1, j, m, n, k, booleanMatrix) +
      dfs(i, j + 1, m, n, k, booleanMatrix) +
      dfs(i, j - 1, m, n, k, booleanMatrix);

    return res;
  }

  function matrixMaker<T>(row: number, col: number, val: T): T[][] {
    const matrix = [];
    for (let i = 0; i < row; i++) {
      const arr = new Array(col);
      arr.fill(val);
      matrix.push(arr);
    }

    return matrix;
  }

  // const test1 = getDigitalSum(12, 3);
  // console.log(test1);

  // const test2 = getDigitalSum(121, 3);
  // console.log(test2);

  function getDigitalSum(n: number) {
    let sum = 0;
    while (n > 0) {
      sum += n % 10;
      n = Math.floor(n / 10);
    }

    return sum;
  }

  (function () {
    function getDigitalSum(i: number, j: number): number {
      const str = i.toString() + j.toString();
      const res = str
        .split('')
        .map((e) => parseInt(e))
        .reduce((pre: number, cur: number): number => {
          return pre + cur;
        });

      return res;
    }
  })();
})();
