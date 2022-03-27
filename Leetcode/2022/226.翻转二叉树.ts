/*
 * @lc app=leetcode.cn id=226 lang=typescript
 *
 * [226] 翻转二叉树
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

function invertTree(root: TreeNode | null): TreeNode | null {
  if (!root) return root;
  const queue = [root];

  while (queue.length) {
    const e = queue.pop()!;

    if (e.right) {
      queue.push(e.right);
    }

    if (e.left) {
      queue.push(e.left);
    }

    [e.left, e.right] = [e.right, e.left];
  }

  return root;
}
// @lc code=end

(function () {
  function invertTree(root: TreeNode | null): TreeNode | null {
    // First invert the leaves node.
    if (!root) return root;

    invertTree(root.left);
    invertTree(root.right);
    [root.left, root.right] = [root.right, root.left];

    return root;
  }
})();
(function () {
  function invertTree(root: TreeNode | null): TreeNode | null {
    if (!root) return root;

    const left = invertTree(root.left);
    const right = invertTree(root.right);
    [root.left, root.right] = [right, left];

    return root;
  }
})();

(function () {
  // First invert the root node.
  function invertTree(root: TreeNode | null): TreeNode | null {
    if (!root) return root;

    [root.left, root.right] = [root.right, root.left];
    invertTree(root.left);
    invertTree(root.right);

    return root;
  }
})();
