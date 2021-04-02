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

var inorderTraversal = function (root) {
  if (!root) return [];
  const left = root.left ? root.left : [];
  const right = root.right ? root.right : [];
  return inorderTraversal(left)
    .concat(root.val)
    .concat(inorderTraversal(right));
};

var inorderTraversal = function (root) {
  if (!root) return [];
  const stack = [root];
  const ret = [];
  let item = null;
  let left = root.left;
  while (left) {
    stack.push(left);
    left = left.left;
  }

  while ((item = stack.pop())) {
    ret.push(item.val);
    let t = item.right;

    while (t) {
      stack.push(t);
      t = t.left;
    }
  }

  return ret;
};
