/*
 * @lc app=leetcode.cn id=145 lang=typescript
 *
 * [145] 二叉树的后序遍历
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

function postorderTraversal(root: TreeNode | null): number[] {
  // Iteration
  // https://leetcode-cn.com/problems/binary-tree-postorder-traversal/solution/die-dai-chao-jian-dan-qian-zhong-hou-er-b79fc/
  if (!root) return [];
  const stack: TreeNode[] = [];
  const res = [];
  while (stack.length || root) {
    while (root) {
      stack.push(root);
      // Store the right subtree traverse result into the end of the array.
      res.unshift(root.val);
      root = root.right;
    }

    root = stack.pop()!;
    root = root.left;
  }

  return res;
}
// @lc code=end

try {
  function postorderTraversal(root: TreeNode | null): number[] {
    // Iteration 2
    // https://leetcode-cn.com/problems/binary-tree-postorder-traversal/solution/er-cha-shu-de-hou-xu-bian-li-by-leetcode-solution/
    if (!root) return [];

    let cur: TreeNode | null = root;
    const res = [];
    const stack: Array<TreeNode> = [];

    // The last node whose value is store to the `res` array;
    let pre = null;

    while (stack.length || cur) {
      // Traverse to the most left node of `cur` node.
      while (cur) {
        stack.push(cur);
        cur = cur.left;
      }

      // In this time, the `cur` stores the most left node.
      cur = stack.pop()!;

      if (cur.right && cur.right !== pre) {
        stack.push(cur);
        cur = cur.right;
      } else {
        // Implicitly conditional: `cur.right === null && cur.left === null`.
        // Only store the value of current node into `res` array,
        // when the left node and right node of current node both are empty.
        res.push(cur.val);
        pre = cur;
        cur = null;
      }
    }

    return res;
  }
} catch (error) {}

try {
  function postorderTraversal(root: TreeNode | null): number[] {
    // Recursion 1
    if (!root) return [];

    return [
      ...postorderTraversal(root.left),
      ...postorderTraversal(root.right),
      root.val,
    ];
  }
} catch (error) {}

try {
  function postorderTraversal(root: TreeNode | null): number[] {
    // Iteration 1; Not directly post order traversal,

    // instead, it traverses in the: middle, right, left order then reverse the result array.
    // https://leetcode-cn.com/problems/binary-tree-postorder-traversal/solution/bang-ni-dui-er-cha-shu-bu-zai-mi-mang-che-di-chi-t/

    if (!root) return [];

    const res = [];
    const stack = [root];
    // The below is a little bit different with pre-order traversal.
    while (stack.length) {
      const top = stack.pop()!;

      res.push(top.val);

      if (top.left) {
        stack.push(top.left);
      }

      if (top.right) {
        stack.push(top.right);
      }
    }

    return res.reverse();
  }
} catch (error) {}
