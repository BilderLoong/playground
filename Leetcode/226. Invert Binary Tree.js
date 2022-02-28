// Recursion
var invertTree = function (root) {
  if (root === null) return root;
  invertTree(root.left);
  invertTree(root.right);
  [root.left, root.right] = [root.right, root.left];
  return root;
};
