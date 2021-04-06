var findDuplicateSubtrees = function (root) {
  const memo = new Map();
  const result = new Set();

  tranverse(root);
  return Array.from(result);

  function tranverse(root) {
    if (!root) return '#';

    const left = tranverse(root.left);
    const right = tranverse(root.right);

    const subtree = `${left},${right},${root.val}`;

    const count = memo.get(subtree);
    if (count === 1) {
      result.add(root);
    }

    memo.set(subtree, (count ?? 0) + 1);

    return subtree;
  }
};
