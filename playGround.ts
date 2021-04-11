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

let root = new TreeNode(0);
let pre = -Infinity;
isValidBST(root);

function isValidBST(root: TreeNode | null): boolean {
  if (!root) return true;

  if (!isValidBST(root.left)) return false;

  if (root.val <= pre) return false;
  pre = root.val;

  return isValidBST(root.right);
}
