(function () {
  // Recursion
  /**
   * @description Return the head of reversed link list (se by inputted node).
   * @param node
   */
  function reverseList(node: ListNode | null): ListNode | null {
    if (!node?.next) {
      return node;
    }
    const newList = reverseList(node.next);
    node.next.next = node;
    node.next = null;

    return newList;
  }
})();

(function () {
  // Iteration 1
  function reverseList(head: ListNode | null): ListNode | null {
    if (!head) return head;

    let cur = head;
    let next = cur.next;

    head.next = null;

    while (next) {
      const temp = next.next;
      next.next = cur;
      cur = next;
      next = temp;
    }

    return cur;
  }
  // Iteration 2
  function reverseList(head: ListNode | null): ListNode | null {
    let cur = head;
    let prev = null;

    while (cur) {
      const next = cur.next;
      cur.next = prev;
      prev = cur;
      cur = next;
    }

    return prev;
  }
})();

(function () {
  // Iteration 1
  function reverseList(head: ListNode | null): ListNode | null {
    if (!head) return head;

    let cur = head;
    let next = cur.next;

    head.next = null;

    while (next) {
      const temp = next.next;
      next.next = cur;
      cur = next;
      next = temp;
    }

    return cur;
  }
})();
