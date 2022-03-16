export default {};
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
var postorderTraversal = function (root) {
  if (!root) return false;
  const stack = [root];
  const ret = [];
  let p = root;
  while (stack.length > 0) {
    const top = stack[stack.length - 1];
    if (
      top.right === p ||
      top.left === p ||
      (top.left === null) & (top.right === null)
    ) {
      p = stack.pop();
      ret.push(p.val);
    } else {
      if (top.right) stack.push(top.right);
      if (top.left) stack.push(top.left);
    }
  }
  return ret;
};
