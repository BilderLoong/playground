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

function inorderTraversal(root) {
  let result = [];
  traverse(root, result);
  return result;

  // Why does use parameter to pass the arr like this: foo(root,result) instead of closure will
  // got result undefined error
  function traverse(root) {
    if (!root) return;

    traverse(root.left);
    result.push(root.val);
    traverse(root.right);
  }
}

//----------------------------------------------------------------
var inorderTraversal = function (root) {
  if (!root) return [];
  return inorderTraversal(root.left)
    .concat(root.val)
    .concat(inorderTraversal(root.right));
};

//----------------------------------------------------------------

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
