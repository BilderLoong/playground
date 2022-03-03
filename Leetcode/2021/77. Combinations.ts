export {};
(function () {
  const res1 = combine(4, 2);
  console.assert(
    JSON.stringify(res1) ===
      JSON.stringify([
        [2, 4],
        [3, 4],
        [2, 3],
        [1, 2],
        [1, 3],
        [1, 4],
      ]),
    res1
  );

  function combine(n: number, k: number): number[][] {
    const res: number[][] = [];
    helper(n, k, 1, [], res);
    return res;
  }

  function helper(
    n: number,
    k: number,
    start: number,
    path: number[],
    res: number[][]
  ) {
    if (path.length === k) {
      res.push([...path]);
      return;
    }

    for (let i = start; i <= n; i++) {
      path.push(i);
      helper(n, k, i + 1, path, res);
      path.pop();
    }
  }
})();
