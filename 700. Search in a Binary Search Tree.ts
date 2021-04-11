function searchBST(root: TreeNode | null, val: number): TreeNode | null {
  while (root && root.val !== val) {
    root = root.val > val ? root.left : root.right;
  }
  return root;
}

function searchBST(root: TreeNode | null, val: number): TreeNode | null {
  while (root) {
    const current = root.val;
    if (current === val) return root;
    else if (current > val) {
      root = root.left;
    } else root = root.right;
  }
  return null;
}

function searchBST(root: TreeNode | null, val: number): TreeNode | null {
  return !root || root.val === val
    ? root
    : root.val > val
    ? searchBST(root.left, val)
    : searchBST(root.right, val);
}
