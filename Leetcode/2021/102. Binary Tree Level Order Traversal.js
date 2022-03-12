(function () {
  var levelOrder = function (root) {
    if (!root) return [];

    let levelNodes = [];
    const queue = [root, null];
    const result = [];

    while (queue.length > 0) {
      const t = queue.shift();
      if (t) {
        levelNodes.push(t.val);

        if (t.left) {
          queue.push(t.left);
        }

        if (t.right) {
          queue.push(t.right);
        }
      } else {
        result.push(levelNodes);
        levelNodes = [];

        if (queue.length > 0) {
          queue.push(null);
        }
      }
    }
    return result;
  };
})();

(function () {
  var levelOrder = function (root) {
    if (!root) return [];
    const queue = [root];
    let levelNodes = [];
    const result = [];

    while (queue.length) {
      // store the length of current level
      let levelLength = queue.length;

      while (levelLength) {
        let node = queue.shift();
        levelNodes.push(node.val);

        node.left && queue.push(node.left);
        node.right && queue.push(node.right);
        levelLength--;
      }

      result.push(levelNodes);
      levelNodes = [];
    }
    return result;
  };
})();
