it('', () => {
  expect(minArray([3, 4, 5, 1, 2])).toBe(1);
  expect(minArray([2, 2, 2, 0, 1])).toBe(0);

  function minArray(numbers: number[]): number {
    let [low, high] = [0, numbers.length - 1];

    while (low < high) {
      const mid = Math.floor((low + high) / 2);

      if (numbers[mid] < numbers[high]) {
        // `mid` locates in right array.
        high = mid;
      } else if (numbers[mid] > numbers[high]) {
        // `mid` locates in left array.
        low = mid + 1;
      } else {
        high--;
      }
    }

    return numbers[low];
  }
});
