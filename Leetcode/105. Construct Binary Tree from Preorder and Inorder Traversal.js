function buildTree(preorder, inorder) {
  const map = new Map();
  inorder.forEach((e, i) => {
    map.set(e, i);
  });

  return build(
    preorder,
    0,
    preorder.length - 1,
    inorder,
    0,
    inorder.length - 1
  );

  function build(preorder, preStart, preEnd, inorder, inStart, inEnd) {
    if (preStart > preEnd) return null;

    const root = new TreeNode(preorder[preStart]);
    const rootIndexOfInorder = map.get(root.val)

    const leftSize = rootIndexOfInorder - inStart;

    root.left = build(
      preorder,
      preStart + 1,
      preStart + leftSize,
      inorder,
      inStart,
      rootIndexOfInorder - 1
    );
    root.right = build(
      preorder,
      preStart + leftSize + 1,
      preEnd,
      inorder,
      rootIndexOfInorder + 1,
      inEnd
    );

    return root;
  }
}
//------------------------------------------------
function buildTree(preorder, inorder) {
  return build(
    preorder,
    0,
    preorder.length - 1,
    inorder,
    0,
    inorder.length - 1
  );

  function build(preorder, preStart, preEnd, inorder, inStart, inEnd) {
    if (preStart > preEnd) return null;

    const root = new TreeNode(preorder[preStart]);
    const rootIndexOfInorder = inorder.indexOf(root.val);

    const leftSize = rootIndexOfInorder - inStart;

    root.left = build(
      preorder,
      preStart + 1,
      preStart + leftSize,
      inorder,
      inStart,
      rootIndexOfInorder - 1
    );
    root.right = build(
      preorder,
      preStart + leftSize + 1,
      preEnd,
      inorder,
      rootIndexOfInorder + 1,
      inEnd
    );

    return root;
  }
}

//----------------------------------------------------------------
function buildTree(preorder, inorder) {
  if (!preorder.length) return null;

  const root = new TreeNode(preorder[0]);
  const rootIndexOfInorder = inorder.indexOf(root);

  root.left = buildTree(
    preorder.slice(1, rootIndexOfInorder + 1),
    inorder.slice(0, rootIndexOfInorder)
  );

  root.right = buildTree(
    preorder.slice(rootIndexOfInorder + 1),
    inorder.slice(rootIndexOfInorder + 1)
  );

  return root;
}
