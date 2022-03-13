export {};
//https://leetcode-cn.com/problems/binary-tree-inorder-traversal/

/**
 * Definition for a binary tree node.
 * function TreeNode(val, left, right) {
 *     this.val = (val===undefined ? 0 : val)
 *     this.left = (left===undefined ? null : left)
 *     this.right = (right===undefined ? null : right)
 * }
 */
/**
 * @param {TreeNode} root
 * @return {number[]}
 */
var preorderTraversal = function (root) {
  if (!root) return [];

  const stack = [];
  const ret = [root];
  let t = root;
  while (t) {
    if (t.right) stack.push(t.right);
    if (t.left) stack.push(t.left);
    ret.push(t.val);
    t = stack.pop();
  }

  return ret;
};
