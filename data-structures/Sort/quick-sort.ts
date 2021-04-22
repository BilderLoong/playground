class QuickSortInPlace {
  static partition(arr: number[], l: number, r: number) {
    const pivot = arr[r];
    let i = l - 1;

    for (let j = l; j <= r; j++) {
      if (arr[j] < pivot) {
        i++;
        [arr[i], arr[j]] = [arr[j], arr[i]];
      }
    }

    [arr[i + 1], arr[r]] = [arr[r], arr[i + 1]];

    return i + 1; //return the index of pivot;
  }
  static quickSort(arr: number[], l: number, r: number) {
    if (l > r) return;
    const indexOfPivot = this.partition(arr, l, r);
    this.quickSort(arr, l, indexOfPivot - 1);
    this.quickSort(arr, indexOfPivot + 1, r);
  }
}
const testArr1 = [0, 1, 4, 9, 0, 8, 4, 0];
const testArr1Result = [0, 0, 0, 1, 4, 4, 8, 9];

QuickSortInPlace.quickSort(testArr1, 0, testArr1.length - 1);
console.assert(testArr1 === testArr1Result, testArr1);
