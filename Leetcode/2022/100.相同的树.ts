/*
 * @lc app=leetcode.cn id=100 lang=typescript
 *
 * [100] 相同的树
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

function isSameTree(p: TreeNode | null, q: TreeNode | null): boolean {
  if (!p && !q) {
    return true;
  }

  if (!p || !q) {
    return false;
  }

  const queue1 = [p],
    queue2 = [q];

  while (queue1.length && queue2.length) {
    const e1 = queue1.pop()!;
    const e2 = queue2.pop()!;

    if (e1.val !== e2.val) {
      return false;
    }

    if (Number(e1.left === null) ^ Number(e2.left === null)) {
      return false;
    }

    if (Number(e1.right === null) ^ Number(e2.right === null)) {
      return false;
    }

    if (e1.left && e2.left) {
      queue1.push(e1.left);
      queue2.push(e2.left);
    }

    if (e1.right && e2.right) {
      queue1.push(e1.right);
      queue2.push(e2.right);
    }
  }

  return true;
}
// @lc code=end

(function () {
  function isSameTree(p: TreeNode | null, q: TreeNode | null): boolean {
    return JSON.stringify(p) === JSON.stringify(q);
  }
})();

(function () {
  function isSameTree(p: TreeNode | null, q: TreeNode | null): boolean {
    // Base case
    if (!p && !q) {
      // Both are null;
      return true;
    } else if (!p || !q) {
      // Only one of them is null;
      return false;
    }

    return (
      p.val === q.val &&
      isSameTree(p.left, q.left) &&
      isSameTree(p.right, q.right)
    );
  }
})();
