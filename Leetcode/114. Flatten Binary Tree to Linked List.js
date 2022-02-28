var flatten = function (root) {
  if (!root) return;

  flatten(root.left);
  flatten(root.right);

  const left = root.left;
  const right = root.right;

  root.left = null;
  root.right = left;

  let endOfRight = root;
  while (endOfRight.right) {
    endOfRight = endOfRight.right;
  }
  endOfRight.right = right;
};
