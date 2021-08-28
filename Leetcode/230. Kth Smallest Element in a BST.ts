function kthSmallest(root: TreeNode | null, k: number): number {
  const arr = build(root);
  return arr[k - 1];

  function build(node: TreeNode): number[] {
    if (!node) return [];

    return [...build(node.left), node.val, ...build(node.right)];
  }
}

console.log(`1`);

//--------------------------------------------------------------------