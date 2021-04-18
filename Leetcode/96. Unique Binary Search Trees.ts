(function () {
  //dp

  const test2 = numTrees(3);
  console.assert(test2 === 5, 'test2 failed with result: %s', test2);

  function numTrees(n: number): number {
    const dp = new Array(n + 1).fill(0);
    dp[0] = 1;
    dp[1] = 1;
    for (let i = 2; i <= n; i++) {
      for (let j = 0; j <= i - 1; j++) {
        dp[i] += dp[j] * dp[i - 1 - j];
      }
    }

    return dp[n];
  }
})();

(function () {
  const test1 = numTrees(2);
  console.assert(test1 === 2, test1);

  const test2 = numTrees(3);
  console.assert(test2 === 5, 'test2 failed with result: %s', test2);

  function numTrees(n: number): number {
    return helper(1, n);
  }

  // output the number of BST which
  // able be structured by using the number of [start, end]
  function helper(start: number, end: number): number {
    if (start > end) return 1;
    if (start === end) return 1;

    let res = 0;
    for (let i = start; i <= end; i++) {
      let left = helper(start, i - 1);
      let right = helper(i + 1, end);
      res += left * right;
    }

    return res;
  }
})();

(function () {
  const test1 = numTrees(2);
  console.assert(test1 === 2, test1);

  const test2 = numTrees(3);
  console.assert(test2 === 5, 'test2 failed with result: %s', test2);

  function numTrees(n: number): number {
    return helper(1, n, {});
  }

  // output the number of BST which
  // able be structured by using the number of [start, end]
  function helper(start: number, end: number, map: any): number {
    if (start > end || start === end) return 1;

    // the number of possible BST only concern with the number of the node,
    // no matter what is the value of node.
    const key = `${start - end}`;
    if (map[key] !== undefined) return map[key];

    let res = 0;
    for (let i = start; i <= end; i++) {
      let left = helper(start, i - 1, map);
      let right = helper(i + 1, end, map);
      res += left * right;
    }

    map[key] = res;

    return res;
  }
})();

(function () {
  const test1 = numTrees(2);
  console.assert(test1 === 2, test1);

  const test2 = numTrees(3);
  console.assert(test2 === 5, 'test2 failed with result: %s', test2);

  function numTrees(n: number): number {
    return helper(n, []);
  }

  // return value is the number of BST which
  // able be structured by using the number of `n`
  function helper(n: number, arr: number[]): number {
    if (n <= 1) return 1;
    if (arr[n]) return arr[n];

    let res = 0;
    for (let i = 0; i <= n - 1; i++) {
      const left = helper(i, arr);
      const right = helper(n - i - 1, arr);
      res += left * right;
    }

    arr[n] = res;

    return res;
  }
})();
