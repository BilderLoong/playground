(function () {
  // Backtrack
  // https://leetcode-cn.com/problems/coin-change/solution/javadi-gui-ji-yi-hua-sou-suo-dong-tai-gui-hua-by-s/
  const coins2: number[] = [];
  const amount2 = 11;
  const res2 = coinChange(coins2, amount2);
  console.assert(res2 === -1, 1.1, res2);

  const coins1 = [1, 2, 5];
  const amount1 = 11;
  const res1 = coinChange(coins1, amount1);
  console.assert(res1 === 3, 1, res1);

  function coinChange(coins: number[], amount: number): number {
    const res = dfs(coins, amount, 0);

    return res === Infinity ? -1 : res;

    function dfs(coins: number[], amount: number, coinCount: number): number {
      if (amount < 0) return Infinity;
      if (amount === 0) return coinCount;

      let min = Infinity;

      for (const coin of coins) {
        min = Math.min(min, dfs(coins, amount - coin, coinCount + 1));
      }

      return min;
    }
  }
})();
(function () {
  //dp
  //https://leetcode-cn.com/problems/coin-change/solution/js-xiang-jie-dong-tai-gui-hua-de-si-xiang-yi-bu-da/

  const coins1 = [1, 2, 5];
  const amount1 = 11;
  const res1 = coinChange(coins1, amount1);
  console.assert(res1 === 3, 2.1, res1);

  function coinChange(coins: number[], amount: number): number {
    const dp = new Array(amount + 1).fill(Infinity);
    dp[0] = 0;

    for (let i = 0; i <= amount; i++) {
      for (const coin of coins) {
        if (i >= coin) {
          dp[i] = Math.min(dp[i - coin] + 1, dp[i]);
        }
      }
    }

    return dp[amount] === Infinity ? -1 : dp[amount];
  }
})();
(function () {
  // my idea:
  // using linear search so search the minium number of coins

  // using canChangeCoin to check whether current number of coins
  // can make up the amount. But I find it's difficulty to list all
  // combinations.

  // I want to get the combination is made by take a set of number and the length of every combination
  // e.g. set = [1,2,5,4] length =  3
  // all the number of combination is: 4*4*4

  const coins1 = [1, 2, 5];
  const amount1 = 11;
  const res1 = coinChange(coins1, amount1);
  console.assert(res1 === 3, 3.1, res1);

  function coinChange(coins: number[], amount: number): number {
    const start = Math.floor(amount / Math.max(...coins));
    const end = Math.ceil(amount / Math.min(...coins));

    for (let i = start; i <= end; i++) {
      if (canChangeCoin(coins, amount, i)) {
        return i;
      }
    }

    return -1;
  }

  function canChangeCoin(
    coins: number[],
    amount: number,
    coinNumber: number
  ): boolean {}
})();
