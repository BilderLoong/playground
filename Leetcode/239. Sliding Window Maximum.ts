// brute force
function maxSlidingWindow(nums: number[], k: number): number[] {
  const res = [];
  for (let i = 0, j = k - 1; j < nums.length; i++, j++) {
    res.push(Math.max(...nums.slice(i, j + 1)));
  }

  return res;
}

let input = [9, 10, 9, -7, -4, -8, 2, -6];
let k = 5;

console.log(maxSlidingWindow(input, k));

// using a queue to store the index of max value
function maxSlidingWindow(nums: number[], k: number): number[] {
  const res = [];
  const queue = []; // store the index

  for (let i = 0; i < k; i++) {
    while (queue.length && nums[i] > nums[queue[queue.length - 1]]) {
      queue.pop();
    }
    queue.push(i);
  }

  res.push(nums[queue[0]]);

  for (let i = k; i < nums.length; i++) {
    // Add the new index to the queue
    while (queue.length && nums[i] > nums[queue[queue.length - 1]]) {
      queue.pop();
    }
    queue.push(i);

    // check whether the max value index is in the current slide window
    while (queue[0] <= i - k) {
      queue.shift();
    }

    res.push(nums[queue[0]]);
  }

  return res;
}
