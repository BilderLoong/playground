(function () {
  // Version 2

  /**
   * @param {Node} head
   * @return {Node}
   */
  var copyRandomList = function (head) {
    if (!head) return null;

    const doubledListHead = doubleLinkList(head);
    // During traversing point the original node.
    let pre = doubledListHead;

    // Copy `random` pointer.
    while (pre) {
      // During traversing point the node of new created list.
      const cur = pre.next;
      // Note that the `pre.random` may point to `null`.
      cur.random = pre.random?.next ?? null;
      pre = cur.next;
    }

    // Decouple list into two.
    pre = doubledListHead;
    const res = pre.next;
    while (pre) {
      const cur = pre.next;
      pre.next = cur.next;
      // Note that the value of `cur.next` may be `null` (the tail node.).
      cur.next = cur.next?.next ?? null;
      pre = pre.next;
    }

    return res;
  };

  function doubleLinkList(node) {
    let cur = node;

    while (cur) {
      cur.next = new Node(cur.val, cur.next, null);
      cur = cur.next.next;
    }

    return node;
  }
})();
(function () {
  // Version 1

  /**
   * @param {Node} head
   */
  var copyRandomList = function (head) {
    if (!head) return null;

    const map = new Map();
    let cur = head;
    while (cur) {
      map.set(cur, new Node(cur.val));
      cur = cur.next;
    }

    cur = head;
    while (cur) {
      // The map doesn't contain { null : null } key-value pair.
      // If don't provide default value (null),
      // the `next` pointer value of new created list tail node is `undefined`.
      map.get(cur).next = map.get(cur.next) || null; // `cur.next` maybe` null`.
      map.get(cur).random = map.get(cur.random);
      cur = cur.next;
    }

    return map.get(head);
  };
})();

(function () {
  // WARNING: Wrong solution.
  /**
   * @param {Node} head
   * @return {Node | null}
   */
  var copyRandomList = function (head) {
    if (!head) return null;

    /**
     * Key: The memory address of old node;
     * Value: The corresponding memory address of new node;
     */
    const map = new Map();

    let tailOnNewList = new Node(head.val, null, null);
    const res = tailOnNewList;

    let curOnOldList = head;
    while (curOnOldList) {
      const newNext = map.get(curOnOldList.next);
      if (newNext) {
        tailOnNewList.next = newNext;
      } else {
        const newNext = curOnOldList.next
          ? new Node(curOnOldList.next.val, null, null)
          : null;

        map.set(curOnOldList.next, newNext);
        tailOnNewList.next = newNext;
      }

      const newRandom = map.get(curOnOldList.random);
      if (newRandom) {
        tailOnNewList.random = newRandom;
      } else {
        const newRandom = curOnOldList.random
          ? new Node(curOnOldList.random.val, null, null)
          : null;
        map.set(curOnOldList.random, newRandom);
        tailOnNewList.random = newRandom;
      }

      curOnOldList = curOnOldList.next;
      // tailOnNewList = map.get(curOnOldList.next);
      tailOnNewList = tailOnNewList.next;
    }

    return res;
  };
})();
