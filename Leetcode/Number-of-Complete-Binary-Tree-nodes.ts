// not validated
function nodeCounter(root: TreeNode | null): number {
  if (!root) return 0;

  let leftNode = root.left;
  let depthOfLeft = 0;

  let rightNode = root.right;
  let depthOfRight = 0;

  while (leftNode) {
    depthOfLeft++;
    leftNode = leftNode.left;
  }

  while (rightNode) {
    depthOfRight++;
    rightNode = rightNode.left;
  }

  if (depthOfLeft === depthOfRight) return 2 ** depthOfLeft - 1;

  return 1 + nodeCounter(root.left) + nodeCounter(root.right);
}
