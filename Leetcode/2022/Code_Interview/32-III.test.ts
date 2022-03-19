(function () {
  // Version 4
  function levelOrder(root: TreeNode | null): number[][] {
    if (!root) return [];

    const res = [];
    const queue = [root];

    while (queue.length) {
      let curLevelNodes = [];

      for (let i = 0, len = queue.length; i < len; i++) {
        const e = queue.shift()!;
        curLevelNodes.push(e.val);

        if (e.left) {
          queue.push(e.left);
        }

        if (e.right) {
          queue.push(e.right);
        }
      }

      res.push(curLevelNodes);
    }

    // Reverse lines with even line number.
    return res.map((e, i) => {
      if (i % 2) {
        // Even line, due to the line number === i + 1.
        return e.reverse();
      } else {
        // Odd
        return e;
      }
    });
  }
})();
(function () {
  // Version 3
  // Similar to version 1, but don't use the `null` as the separator for each level.
  function levelOrder(root: TreeNode | null): number[][] {
    if (!root) return [];

    const res = [];
    const queue = [root];

    while (queue.length) {
      let curLevelNodes = [];

      for (let i = 0, len = queue.length; i < len; i++) {
        const e = queue.shift()!;
        curLevelNodes.push(e.val);

        if (e.left) {
          queue.push(e.left);
        }

        if (e.right) {
          queue.push(e.right);
        }
      }

      res.push(curLevelNodes);
      if (!queue.length) {
        break;
      }
      curLevelNodes = [];

      for (let i = 0, len = queue.length; i < len; i++) {
        const e = queue.pop()!;
        curLevelNodes.push(e.val);

        if (e.right) {
          queue.unshift(e.right);
        }

        if (e.left) {
          queue.unshift(e.left);
        }
      }

      res.push(curLevelNodes);
    }

    return res;
  }
})();

(function () {
  // Version 2
  function levelOrder(root: TreeNode | null): number[][] {
    if (!root) return [];

    const res = [];
    let curLevelNodes = [];
    // null mark the end of one level.
    const queue = [root, null];

    while (queue.length) {
      const e = queue.shift();

      if (e) {
        if (res.length % 2 === 0) {
          curLevelNodes.push(e.val);
        } else {
          curLevelNodes.unshift(e.val);
        }

        if (e.left) {
          queue.push(e.left);
        }

        if (e.right) {
          queue.push(e.right);
        }
      } else {
        res.push(curLevelNodes);
        curLevelNodes = [];

        if (queue.length) {
          queue.push(null);
        }
      }
    }

    return res;
  }
})();
(function () {
  // Version 1
  function levelOrder(root: TreeNode | null): number[][] {
    if (!root) return [];

    const res = [];
    // null mark the end of one level.
    const queue = [root, null];

    while (queue.length) {
      let curLevelNodes = [];

      for (let e = queue.shift(); e; e = queue.shift()) {
        curLevelNodes.push(e.val);

        if (e.left) {
          queue.push(e.left);
        }

        if (e.right) {
          queue.push(e.right);
        }
      }

      res.push(curLevelNodes);

      if (!queue.length) {
        break;
      }

      if (queue.length) {
        queue.push(null);
      }

      curLevelNodes = [];

      for (let e = queue.shift(); e; e = queue.shift()) {
        // Difference from above for loop.
        curLevelNodes.unshift(e.val);

        if (e.left) {
          queue.push(e.left);
        }

        if (e.right) {
          queue.push(e.right);
        }
      }

      if (queue.length) {
        queue.push(null);
      }

      res.push(curLevelNodes);
    }

    return res;
  }
})();
