/*
 * @lc app=leetcode.cn id=102 lang=typescript
 *
 * [102] 二叉树的层序遍历
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

function levelOrder(root: TreeNode | null): number[][] {
  const res = [];

  if (!root) {
    return [];
  }

  const queue = [root];
  while (queue.length) {
    let curLevel = [];
    const curLevelSize = queue.length;

    for (let i = 0; i < curLevelSize; i++) {
      const e = queue.shift()!;
      curLevel.push(e.val);

      if (e.left) {
        queue.push(e.left);
      }

      if (e.right) {
        queue.push(e.right);
      }
    }

    res.push(curLevel);
    curLevel = [];
  }

  return res;
}

// @lc code=end

(function () {
  function levelOrder(root: TreeNode | null): number[][] {
    if (!root) return [];

    const res = [];
    let curLevelNodes = [];
    // null mark the end of one level.
    const queue = [root, null];

    while (queue.length) {
      const e = queue.shift();

      if (e) {
        curLevelNodes.push(e.val);

        if (e.left) {
          queue.push(e.left);
        }

        if (e.right) {
          queue.push(e.right);
        }
      } else {
        res.push(curLevelNodes);
        curLevelNodes = [];

        if (queue.length) {
          queue.push(null);
        }
      }
    }

    return res;
  }
})();
