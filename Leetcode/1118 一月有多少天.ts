export {};
function dailyTemperatures(arr: number[]): number[] {
  const stack: number[] = [];
  const res: number[] = [];

  for (let i = arr.length - 1; i >= 0; i--) {
    while (stack.length && arr[i] >= arr[stack[stack.length - 1]]) {
      stack.pop();
    }

    res[i] = stack.length ? stack[stack.length - 1] - i : 0;
    stack.push(i);
  }

  return res;
}

function dailyTemperatures(arr: number[]): number[] {
  const stack: number[] = [];
  const res: number[] = [];

  arr.forEach((e, i) => {
    const top = stack[stack.length - 1];

    while (stack.length && e >= arr[top]) {
      // when the elements in the stack popped is get
      // the current `e` is the next warmer day
      const a = stack.pop()!;
      res[a] = i - a;
    }

    stack.push(i);
  });

  stack.forEach((e) => (res[e] = 0));

  return res;
}

const test = [73, 74, 75, 71, 69, 76];
const result = [1, 1, 3, 2, 1, 0];

console.log(dailyTemperatures(test));
