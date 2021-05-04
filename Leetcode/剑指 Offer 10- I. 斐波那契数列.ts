function fib(n: number): number {
  if (n < 2) return n;

  let a = 0;
  let b = 1;
  let res = 1;

  for (let i = 2; i <= n; i++) {
    // [a, b, res] = [b, res, a + b];
    res = (a + b) % 1000000007;
    a = b;
    b = res;
  }

  return res;
}

console.log(fib(5));
console.log(fib(2));
console.log(fib(45));
