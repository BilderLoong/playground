function convertBST(root: TreeNode | null): TreeNode | null {
  let sum = 0;

  const traverse = (node: TreeNode | null): TreeNode | null => {
    if (!node) return;

    traverse(node.right);
    node.val += sum;
    sum = node.val;
    traverse(node.left);
  };

  traverse(root);
  return root;
}
