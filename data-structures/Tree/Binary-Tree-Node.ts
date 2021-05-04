export default class BinaryTreeNode {
  val: any;
  left: BinaryTreeNode | null;
  right: BinaryTreeNode | null;
  parent: BinaryTreeNode | null;
  constructor(
    val = null,
    left: BinaryTreeNode,
    right: BinaryTreeNode,
    parent: BinaryTreeNode
  ) {
    this.val = val;
    this.left = left ?? null;
    this.right = right ?? null;
    this.parent = parent ?? null;
  }
  
}
