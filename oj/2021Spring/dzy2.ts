export function nextMoreCustomersInterval(customers: number[]): number[] {
  const res = Array(customers.length).fill(0, 0, customers.length);
  const stack: number[] = [];

  customers.forEach((e, i) => {
    while (stack.length && e > customers[stack[stack.length - 1]]) {
      const pre = stack.pop()!;
      res[pre] = i - pre;
    }

    stack.push(i);
  });

  return res;
}

console.log(
  nextMoreCustomersInterval([199, 221, 230, 212, 210, 225, 245, 255, 240])
);
