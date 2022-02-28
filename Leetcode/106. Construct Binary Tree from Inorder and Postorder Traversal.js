function buildTree(inorder, postorder) {
  //   const map = new Map();
  //   inorder.forEach((e, i) => {
  //     map.set(e, i);
  //   });

  const map = {};
  inorder.forEach((e, i) => {
    map[e] = i;
  });

  return build(
    inorder,
    0,
    inorder.length - 1,
    postorder,
    0,
    postorder.length - 1
  );

  function build(inorder, inStart, inEnd, postorder, postStart, postEnd) {
    if (inStart > inEnd) return null;

    const root = new TreeNode(postorder[postEnd]);
    // const rootIndexofInorder = map.get(root.val);
    const rootIndexofInorder = map[root.val];

    const sizeOfLeft = rootIndexofInorder - inStart;

    root.left = build(
      inorder,
      inStart,
      rootIndexofInorder - 1,
      postorder,
      postStart,
      postStart + sizeOfLeft - 1
    );
    root.right = build(
      inorder,
      rootIndexofInorder + 1,
      inEnd,
      postorder,
      postStart + sizeOfLeft,
      postEnd - 1
    );

    return root;
  }
}
