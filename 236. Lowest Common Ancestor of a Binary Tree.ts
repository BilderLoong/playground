class TreeNode {
  val: number;
  left: TreeNode | null;
  right: TreeNode | null;
  constructor(val?: number, left?: TreeNode | null, right?: TreeNode | null) {
    this.val = val === undefined ? 0 : val;
    this.left = left === undefined ? null : left;
    this.right = right === undefined ? null : right;
  }
}

function lowestCommonAncestor(
  root: TreeNode | null,
  p: TreeNode | null,
  q: TreeNode | null
): TreeNode | null {
  let ans: TreeNode | null;
  dfs(root, p, q);
  return ans;

  function dfs(
    root: TreeNode | null,
    p: TreeNode | null,
    q: TreeNode | null
  ): boolean {
    if (!root) return false;

    const left = dfs(root.left, p, q);
    const right = dfs(root.right, p, q);
    if (
      (left && right) ||
      ((root.val === p.val || root.val === q.val) && (left || right))
    ) {
      ans = root;
    }

    return left || right || root.val === p.val || root.val === q.val;
  }
}
