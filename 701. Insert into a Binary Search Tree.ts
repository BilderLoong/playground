function insertIntoBST(root: TreeNode | null, val: number): TreeNode | null {
  if (!root) return new TreeNode(val);
  let curNode = root;

  while (true) {
    if (curNode.val > val) {
      if (curNode.left) curNode = curNode.left;
      else {
        curNode.left = new TreeNode(val);
        return root;
      }
    } else {
      if (curNode.right) curNode = curNode.right;
      else {
        curNode.right = new TreeNode(val);
        return root;
      }
    }
  }
}

function insertIntoBST(root: TreeNode | null, val: number): TreeNode | null {
  if (!root) return new TreeNode(val);

  let curNode = root;
  let parentNode: TreeNode;
  let direction: 'right' | 'left';

  while (curNode) {
    parentNode = curNode;
    if (curNode.val > val) {
      curNode = curNode.left;
      direction = 'left';
    } else {
      curNode = curNode.right;
      direction = 'right';
    }
  }

  parentNode![direction!] = new TreeNode(val);

  return root;
}
