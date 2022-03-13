/*
 * @lc app=leetcode.cn id=144 lang=typescript
 *
 * [144] 二叉树的前序遍历
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

function preorderTraversal(root: TreeNode | null): number[] {
  // Iteration
  if (!root) return [];

  const res = [];
  const stack = [root];
  while (stack.length) {
    const top = stack.pop()!;

    res.push(top.val);

    if (top.right) {
      stack.push(top.right);
    }

    if (top.left) {
      stack.push(top.left);
    }
  }

  return res;
}
// @lc code=end

try {
  function preorderTraversal(root: TreeNode | null): number[] {
    // Recursion with helper function.
    const res: number[] = [];

    const dfs = (root: TreeNode | null) => {
      if (!root) return;

      res.push(root.val);
      root.left && dfs(root.left);
      root.right && dfs(root.right);
    };

    dfs(root);

    return res;
  }
} catch (error) {}

try {
  function preorderTraversal(root: TreeNode | null): number[] {
    // Recursion 1
    if (!root) return [];

    return [
      root.val,
      ...preorderTraversal(root.left),
      ...preorderTraversal(root.right),
    ];
  }
} catch (err) {}
