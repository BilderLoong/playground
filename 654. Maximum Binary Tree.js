var constructMaximumBinaryTree = function (nums) {
  if (!nums.length) return null;

  const max = Math.max(...nums);
  const indexOfMax = nums.indexOf(max);
  const root = new TreeNode(max);

  const left = constructMaximumBinaryTree(nums.slice(0, indexOfMax));
  const right = constructMaximumBinaryTree(nums.slice(indexOfMax + 1));

  root.right = right;
  root.left = left;

  return root;
};
