export default class QuickSortInPlace {
  static partition(arr: number[], l: number, r: number) {
    const pivot = arr[r];

    // make sure all the elements locate in the [0,i] is
    // less than pivot
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


