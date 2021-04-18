function isValidBST(root: TreeNode | null): boolean {
  function traverse(root: TreeNode | null): number[] {
    if (!root) return [];
    return [...traverse(root.left), root.val, ...traverse(root.right)];
  }

  return traverse(root).every((e, i, arr): boolean =>
    i !== arr.length - 1 ? e < arr[i + 1] : true
  );
}

function isValidBST(root: TreeNode | null): boolean {
  let pre: number = -Infinity;
  let result = true;

  traverse(root);

  return result;

  function traverse(root: TreeNode | null) {
    if (result && root) {
      traverse(root.left);

      if (root.val < pre) result = false;
      pre = root.val;

      traverse(root.right);
    }
  }
}

// can't pass the input of [0] on the leetcode but it passed locally on my mechine
let pre = -Infinity;
function isValidBST(root: TreeNode | null): boolean {
  if (!root) return true;

  if (!isValidBST(root.left)) return false;

  if (root.val <= pre) return false;
  pre = root.val;

  return isValidBST(root.right);
}

//------------------------------------------------------------------

function isValidBST(root: TreeNode | null): boolean {
  const traverse = (
    root: TreeNode | null,
    max: number,
    min: number
  ): boolean => {
    if (!root) return true;

    if (root.val >= max || root.val <= min) return false;

    return (
      traverse(root.left, root.val, min) && traverse(root.right, max, root.val)
    );
  };

  return traverse(root, Infinity, -Infinity);
}

//
function isValidBST(root: TreeNode | null): boolean {
  const stack: TreeNode[] = [];
  let inOrder = -Infinity;
  while (root || stack.length) {
    while (root) {
      stack.push(root);
      root = root.left;
    }

    // Using type assertion. Because the stack.pop() won't return undefined due to the check of while loop
    // so I casting the type of stack.pop() to the TreeNode instead of TreeNode | undefined
    root = stack.pop() as TreeNode;

    if (root.val <= inOrder) return false;

    inOrder = root.val;
    root = root.right;
  }

  return true;
}
