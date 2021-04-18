function lowestCommonAncestor(
  root: TreeNode | null,
  p: TreeNode | null,
  q: TreeNode | null
): TreeNode | null {
  if (!root) return null;
  if (root === q || root === p) return root;

  const left = lowestCommonAncestor(root.left, p, q);
  const right = lowestCommonAncestor(root.right, p, q);

  // if current root is the LCA
  if ((left && right) || ((p === root || q === root) && (left || right))) {
    return root;
  }

  // 1. current root only contain p or q in one of it's left child or right child
  // 2. the child of current root not contain any of p or q
  return left ?? right;
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
