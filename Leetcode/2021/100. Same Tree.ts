export {};
/**
 * Definition for a binary tree node.
 * class TreeNode {
 *     val: number
 *     left: TreeNode | null
 *     right: TreeNode | null
 *     constructor(val?: number, left?: TreeNode | null, right?: TreeNode | null) {
 *         this.val = (val===undefined ? 0 : val)
 *         this.left = (left===undefined ? null : left)
 *         this.right = (right===undefined ? null : right)
 *     }
 * }
 */
// serialize tree
function isSameTree(p: TreeNode | null, q: TreeNode | null): boolean {
  return serialize(p) === serialize(q);

  function serialize(root: TreeNode | null): string {
    if (!root) return '#';

    const left = serialize(root.left);
    const right = serialize(root.right);
    return root.val + left + right;
  }
}

// recursion traverse
(function () {
  function isSameTree(p: TreeNode | null, q: TreeNode | null): boolean {
    return check(p, q);
    function check(p: TreeNode | null, q: TreeNode | null): boolean {
      if (!p && !q) return true;
      if (!p || !q) return false;

      const left = check(p.left, q.left);
      const right = check(p.right, q.right);

      return p.val === q.val && left && right;
    }
  }
});
