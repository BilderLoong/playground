it('should ', () => {
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

  function levelOrder(root: TreeNode | null): number[] {
    if (!root) return [];
    const queue = [root];
    const res = [];

    while (queue.length) {
      const head = queue.shift()!;
      res.push(head.val);
      if (head.left) queue.push(head.left);
      if (head.right) queue.push(head.right);
    }

    return res;
  }
});
