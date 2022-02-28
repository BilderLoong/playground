export default function heapSort(arr: number[]) {
  maxHeapBuilder(arr);
  for (let i = arr.length - 1; i >= 0; i--) {
    [arr[0], arr[i]] = [arr[i], arr[0]];
    maxHeapify(arr, 0, i);
  }
}

export function maxHeapBuilder(arr: number[]) {
  for (let i = Math.floor(arr.length / 2); i >= 0; i--) {
    maxHeapify(arr, i, arr.length);
  }
}

export function maxHeapify(arr: number[], root: number, n: number) {
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