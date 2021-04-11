var TreeNode = /** @class */ (function () {
  function TreeNode(val, left, right) {
    this.val = val === undefined ? 0 : val;
    this.left = left === undefined ? null : left;
    this.right = right === undefined ? null : right;
  }
  return TreeNode;
})();

var root = new TreeNode(0);

var pre = -Infinity;

console.log(isValidBST(root));
function isValidBST(root) {
  if (!root) return true;
  if (!isValidBST(root.left)) return false;
  if (root.val <= pre) return false;
  pre = root.val;
  return isValidBST(root.right);
}
