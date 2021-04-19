var serialize = function (root) {
  const result = [];
  traverse(root);
  return JSON.stringify(result);

  function traverse(root) {
    if (!root) {
      result.push('#');
      return;
    }

    traverse(root.left);
    traverse(root.right);

    result.push(root.val);
  }
};

var deserialize = function (data) {
  const nodes = JSON.parse(data);

  return build(nodes);

  function build(nodes) {
    const val = nodes.pop();

    if (val === '#') return null;

    const root = new TreeNode(val);

    root.right = build(nodes);
    root.left = build(nodes);

    return root;
  }
};
