class LeetCodeTreeNode {
  val: any;
  left: LeetCodeTreeNode | null;
  right: LeetCodeTreeNode | null;

  constructor(val: any) {
    this.val = val;
    this.left = this.right = null;
  }

  static serialize(tree: LeetCodeTreeNode | null) {
    const res = [];
    const queue = [tree];
    while (queue.length > 0) {
      let node = queue.shift();
      if (node) {
        queue.push(node.left);
        queue.push(node.right);
        res.push(node.val);
      } else {
        res.push(null);
      }
    }

    return res;
  }

  static create(arr: number[]) {
    const root = new LeetCodeTreeNode(arr[0]);
    arr.shift();

    const queue = [root];
    while (queue.length > 0) {
      let node = queue.shift();
      // 第一个是左节点，节点为空，直接跳过
      let leftVal = arr.shift();
      if (leftVal != null) {
        node.left = new LeetCodeTreeNode(leftVal);
        queue.push(node.left);
      }
      // 第二个是右节点，节点为空，直接跳过
      let rightVal = arr.shift();
      if (rightVal != null) {
        node.right = new LeetCodeTreeNode(rightVal);
        queue.push(node.right);
      }
      // console.log(queue)
    }

    return root;
  }
}

module.exports = LeetCodeTreeNode;
