var serialize = function (root) {
  const result = [];
  traverse(root);
  return JSON.stringify(result);

  function traverse(root) {
    if (!root) {
      result.push('#');
      return;
    }

    result.push(root.val);

    traverse(root.left);
    traverse(root.right);
  }
};


 // why this function can't function well?
 var serialize = function (root) {
  return JSON.stringify(traverse(root));

  function traverse(root) {
    if (!root) return [];

    return [root.val, ...traverse(root.left), ...traverse(root.right)];
  }
};

var deserialize = function (data) {
  const nodes = JSON.parse(data);

  return build(nodes);

  function build(nodes) {
    /* why don't need to add the below condition check
    if (!nodes.length) return null;
    */
    const val = nodes.shift();

    if (val === '#') return null;

    const root = new TreeNode(val);

    root.left = build(nodes);
    root.right = build(nodes);

    return root;
  }
};

/**
 * Your functions will be called as such:
 * deserialize(serialize(root));
 */
