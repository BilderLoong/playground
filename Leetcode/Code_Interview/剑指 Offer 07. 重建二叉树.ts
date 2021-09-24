import { default as TreeNode } from './LeetCode-TreeNode';

const res = buildTree([3, 9, 20, 15, 7], [9, 3, 15, 20, 7]);
console.log(TreeNode.serialize(res));

// recursion everytime pass new array 
function buildTree(preorder: number[], inorder: number[]): TreeNode | null {
  if (!preorder.length) return null;
  if (preorder.length === 1) return new TreeNode(preorder[0]);

  const root = new TreeNode(preorder[0]);
  const indexOfRoot = inorder.indexOf(preorder[0]);

  const leftInorder = inorder.slice(0, indexOfRoot);
  const rightInorder = inorder.slice(indexOfRoot + 1);

  const leftPreorder = preorder.slice(1, indexOfRoot + 1);
  const rightPreorder = preorder.slice(indexOfRoot + 1);

  root.left = buildTree(leftPreorder, leftInorder);
  root.right = buildTree(rightPreorder, rightInorder);

  return root;
}
