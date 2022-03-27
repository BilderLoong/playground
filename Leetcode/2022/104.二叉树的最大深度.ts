/*
 * @lc app=leetcode.cn id=104 lang=typescript
 *
 * [104] 二叉树的最大深度
 */

// @lc code=start
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

function maxDepth(root: TreeNode | null): number {
  if (!root) {
    return 0;
  }

  let res = 0;
  const queue = [root];
  while (queue.length) {
    res++;
    const len = queue.length;
    for (let i = 0; i < len; i++) {
      const e = queue.shift()!;

      if (e.left) {
        queue.push(e.left);
      }

      if (e.right) {
        queue.push(e.right);
      }
    }
  }

  return res;
}
// @lc code=end

(function () {
  function maxDepth(root: TreeNode | null): number {
    if (!root) {
      return 0;
    }
    return Math.max(maxDepth(root.left), maxDepth(root.right)) + 1;
  }
})();

(function () {
  function maxDepth(root: TreeNode | null): number {
    let max = -Infinity;

    const traverse = (node: TreeNode | null, depth: number) => {
      if (!node) {
        max = Math.max(depth, max);
        return;
      }

      traverse(node.left, depth + 1);
      traverse(node.right, depth + 1);
    };

    traverse(root, 1);

    return max;
  }
})();
