export {}
const TreeNode = require('./LeetCode-TreeNode');

let tree = TreeNode.create([5, 3, 6, 2, 4, null, 7]); // false
tree = deleteNode(tree, 3);
console.log(TreeNode.serialize(tree));

// console.log(TreeNode.serialize(deleteNode(tree, 3)));

function deleteNode(root: TreeNode | null, key: number): TreeNode | null {
  if (!root) return null;

  if (root.val > key) root.left = deleteNode(root.left, key);
  if (root.val < key) root.right = deleteNode(root.right, key);

  if (root.val === key) {
    if (!root.left && !root.right) root = null;
    else if (root.left) {
      const predecessor = getPredecessorNode(root);
      root.val = predecessor;
      root.left = deleteNode(root.left, predecessor);
    } else {
      const successor = getSuccessorNode(root);
      root.val = successor;
      root.right = deleteNode(root.right, successor);
    }
  }

  return root;
}

function getSuccessorNode(root: TreeNode) {
  root = root.right!;
  while (root.left) root = root.left;
  return root.val;
}

function getPredecessorNode(root: TreeNode) {
  root = root.left!;
  while (root.right) root = root.right;

  return root.val;
}

// wrong, to debug
function deleteNode(root: TreeNode | null, key: number): TreeNode | null {
  if (!root) return null;

  if (root.val == key) {
    // the node to be deleted has one or zero child
    if (!root.left) return root.right;
    if (!root.right) return root.left;

    // the node to be deleted has two child
    let predecessorVal = getPredecessorNode(root);
    root.val = predecessorVal;
    root.left = deleteNode(root.left, predecessorVal);
  }

  if (root.val > key) root.left = deleteNode(root.left, key);
  if (root.val < key) root.right = deleteNode(root.right, key);

  return root;
}
