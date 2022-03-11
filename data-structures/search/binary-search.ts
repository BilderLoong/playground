// Recursion
function binarySearchRecursion(
  array: number[],
  target: number,
  start: number,
  end: number
): number | boolean {
  if (start > end) return false;

  const middle = Math.floor((end + start) / 2);
  const guess = array[middle];
  if (guess === target) {
    return middle;
  } else if (guess < target) {
    return binarySearchRecursion(array, target, middle + 1, end);
  } else {
    return binarySearchRecursion(array, target, start, middle - 1);
  }
}

// iteration
function binarySearchIteration(array: number[], target: number) {
  let [low, high] = [0, array.length - 1];
  while (low <= high) {
    const middle = Math.floor((high + low) / 2);
    const guess = array[middle];

    if (guess > target) {
      high = middle - 1;
    } else if (guess < target) {
      low = middle + 1;
    } else {
      return middle;
    }
  }
  return false;
}

const arr = [1, 2, 3, 4, 6, 8, 9];
console.log(binarySearchIteration(arr, 38));
console.log(binarySearchRecursion(arr, 3, 0, arr.length - 1));
