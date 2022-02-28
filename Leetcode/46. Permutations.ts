export {};
const nums1 = [1, 2, 3];
const res1 = permute(nums1);
console.log(res1);

function permute(nums: number[]): number[][] {
  if (!nums.length) return [];

  const res: number[][] = [];
  const size = nums.length;

  dfs([], nums, [], size, 0);

  return res;

  function dfs(
    path: number[],
    nums: number[],
    used: undefined[] | boolean[],
    size: number,
    depth: number
  ): void {
    if (path.length === size) {
      res.push([...path]);
      return;
    }

    for (let i = 0; i < size; i++) {
      if (!used[i]) {
        used[i] = true;
        path.push(nums[i]);

        dfs(path, nums, used, size, depth + 1);

        // Recover to the before state but not really exactly same the before state: the `i` is not the same.
        // and it should be different otherwise the code will yield the same result like before and run into infinite loop
        used[i] = false;
        path.pop();
      }
    }
  }
}
