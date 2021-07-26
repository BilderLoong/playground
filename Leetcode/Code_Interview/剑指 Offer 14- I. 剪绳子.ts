(function () {
  // Greed

  const res3 = cuttingRope(4);
  console.assert(res3 === 4, res3);

  const res1 = cuttingRope(10);
  console.assert(res1 === 36, res1);

  const res2 = cuttingRope(29);
  console.assert(res2 === 39366, res2);

  function cuttingRope(n: number): number {
    if (n === 2) return 1;
    if (n === 3) return 2;

    const times = Math.floor(n / 3);
    const reminder = n % 3;

    switch (reminder) {
      case 0:
        return 3 ** times;

      case 1:
        return 3 ** (times - 1) * 4;

      case 2:
        return 3 ** times * 2;
    }
  }
})();
(function () {
  // dp

  const res3 = cuttingRope(4);
  console.assert(res3 === 4, res3);

  const res1 = cuttingRope(10);
  console.assert(res1 === 36, res1);

  const res2 = cuttingRope(29);
  console.assert(res2 === 39366, res2);

  function cuttingRope(n: number): number {
    const dp = [0, 1, 1];

    for (let i = 3; i <= n; i++) {
      for (let j = 1; j < i; j++) {
        dp[i] ??= -Infinity;
        dp[i] = Math.max(dp[i], (i - j) * j, j * dp[i - j]);
      }
    }

    return dp[n];
  }
})();
(function () {
  // memorize
  const res1 = cuttingRope(10);
  console.assert(res1 === 36, res1);

  function cuttingRope(n: number): number {
    const map: Map<number, number> = new Map();

    return helper(n, map);
  }
  function helper(n: number, map: Map<number, number>): number {
    if (n < 2) return 0;
    if (n === 2) return 1;
    if (n === 3) return 2;

    if (map.has(n)) return map.get(n)!;

    let max = -Infinity;

    for (let i = 1; i < n; i++) {
      max = Math.max(max, Math.max(i * cuttingRope(n - i), i * (n - i)));
    }

    map.set(n, max);

    return max;
  }
})();
(function () {
  // Brute force recursion
  const res1 = cuttingRope(10);
  console.assert(res1 === 36, res1);

  function cuttingRope(n: number): number {
    if (n < 2) return 0;
    if (n === 2) return 1;
    if (n === 3) return 2;

    let max = -Infinity;

    for (let i = 1; i < n; i++) {
      // pay attention to the `i*(n - i)`.
      // because the `i * cuttingRope(n - i)`
      // only consider the case future cut the `n - i` into more small rope,
      // fail to consider the situation not continue cut the  `n - i`.
      // e.g.: the return value cuttingRope(4) need be the max of below case:
      //      i*cuttingRope(n-i) | i*(n-i)
      //
      //      1*cuttingRope(3)   | 1*3
      //      2*cuttingRope(2)   | 2*2
      //      3*cuttingRope(1)   | 3*1

      max = Math.max(max, Math.max(i * cuttingRope(n - i), i * (n - i)));
    }

    return max;
  }
})();
