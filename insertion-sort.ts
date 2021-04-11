function insertionSort(arr: number[]) {
  for (let i = 1; i <= arr.length - 1; i++) {
    for (let j = i; j > 0 && arr[j] < arr[j - 1]; j--) {
      [arr[j - 1], arr[j]] = [arr[j], arr[j - 1]];
    }
  }

  return arr;
}

console.log(insertionSort([12, 11, 13, 0, 6]));
