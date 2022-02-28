import heapSort from '../heap-sort';
import QuickSortInPlace from '../quick-sort';

describe('heapSort', () => {
  test('should sort the array ', () => {
    const testArr = [0, 1, 4, 9, 0, 8, 4, 0];
    heapSort(testArr);
    expect(testArr).toEqual([0, 0, 0, 1, 4, 4, 8, 9]);
  });
});

describe('quickSort', () => {
  test('should sort the array ', () => {
    const testArr1 = [0, 1, 4, 9, 0, 8, 4, 0];
    QuickSortInPlace.quickSort(testArr1, 0, testArr1.length - 1);

    expect(testArr1).toEqual([0, 0, 0, 1, 4, 4, 8, 9]);
  });
});
