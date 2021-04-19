/* This solution is wrong but 
I can't find what is difference with
the right solution: (https://leetcode-cn.com/problems/serialize-and-deserialize-binary-tree/solution/shou-hui-tu-jie-gei-chu-dfshe-bfsliang-chong-jie-f/) */

var serialize = function (root) {
  if (!root) return [];

  const queue = [root];
  const result = [];

  while (queue.length) {
    const node = queue.shift();
    if (node) {
      queue.push(node.right);
      queue.push(node.left);
      result.push(node.val);
    } else {
      result.push('#');
    }
  }

  return JSON.stringify(result);
};

var deserialize = function (data) {
  const nodes = JSON.parse(data);
  // const nodes = data.split(',')

  const root = new TreeNode(nodes[0]);
  const queue = [root];
  let i = 1;

  while (i < nodes.length) {
    const parent = queue.shift();

    const leftVal = nodes[i++];
    if (leftVal != '#') {
      const leftNode = new TreeNode(leftVal);
      parent.left = leftNode;
      queue.push(leftNode);
    }

    const rightVal = nodes[i++];

    if (rightVal != '#') {
      const rightNode = new TreeNode(rightVal);
      parent.right = rightNode;
      queue.push(rightNode);
    }
  }

  return root;
};

/**
 * Your functions will be called as such:
 * deserialize(serialize(root));
 */
