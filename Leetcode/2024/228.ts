function summaryRanges(nums: number[]): string[] {
  const first = nums[0];
  const isContinuous = (start: number, end: number) => end - start === 1;

  return nums
    .slice(1)
    .reduce<Array<[start: number, end: number] | [start: number]>>(
      (pre, cur, index) => {
        const last = pre[pre.length - 1];
        const [start, end] = last.length === 2 ? last : [last[0], last[0]];
        return isContinuous(end, cur)
          ? [...pre.slice(0, -1), [start, cur]]
          : [...pre, [cur]];
      },
      [[first]]
    )
    .map((range) => {
      return range.length === 1
        ? `${range[0]}`
        : range.length === 2
        ? `${range[0]}->${range[1]}`
        : "";
    });
}

console.log(summaryRanges([1, 2, 3, 5, 6, 0, 2]));
