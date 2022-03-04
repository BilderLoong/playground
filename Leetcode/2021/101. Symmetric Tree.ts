// recursion
function isSymmetric(root: TreeNode | null): boolean {
  return check(root, root);
}

function check(p: TreeNode | null, q: TreeNode | null): boolean {
  if (!q && !p) return true;
  else if (!q || !p) return false;

  return q.val === p.val && check(p.left, q.right) && check(p.right, q.left);
}
