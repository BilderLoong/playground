const test4 = [4, 10, 3, 5, 1]; // 10 5 3 4 1

function heapSort(arr: number[]) {
  const res: number[] = [];
  maxHeapBuilder(arr);
  for (let i = arr.length - 1; i >= 0; i--) {
    [arr[0], arr[i]] = [arr[i], arr[0]];
    maxHeapify(arr, 0, i);
  }
}

heapSort(test4);
console.log(test4);

function maxHeapBuilder(arr: number[]) {
  for (let i = Math.floor(arr.length / 2); i >= 0; i--) {
    maxHeapify(arr, i, arr.length);
  }
}

function maxHeapify(arr: number[], root: number, n: number) {
  const left = root * 2 + 1;
  const right = left + 1;
  let largest = root;

  if (left < n && arr[left] > arr[largest]) {
    largest = left;
  }

  if (right < n && arr[right] > arr[largest]) {
    largest = right;
  }

  if (largest !== root) {
    [arr[root], arr[largest]] = [arr[largest], arr[root]];
    maxHeapify(arr, largest, n);
  }
}

const test1 = [4, 2, 8];
const test2 = [8, 2, 4];
const test3 = [2, 8, 4];
maxHeapify(test1, 0);
maxHeapify(test2, 0);
maxHeapify(test3, 0);
console.log(`test case one: ${test1}`);
console.log(`test case two: ${test2}`);
console.log(`test case three: ${test3}`);
