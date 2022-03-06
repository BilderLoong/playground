/**
 * Definition for singly-linked list.
 * class ListNode {
 *     val: number
 *     next: ListNode | null
 *     constructor(val?: number, next?: ListNode | null) {
 *         this.val = (val===undefined ? 0 : val)
 *         this.next = (next===undefined ? null : next)
 *     }
 * }
 */

// Recursion
(function () {
  function reversePrint(head: ListNode | null): number[] {
    if (head === null) return [];

    const res = reversePrint(head.next);
    res.push(head.val);
    return res;
  }
})();

// Iteration
(function () {
  function reversePrint(head: ListNode | null): number[] {
    const stack = [];
    let cur = head;
    while (cur) {
      stack.push(cur.val);
      cur = cur.next;
    }

    stack.reverse();
    return stack;
  }
})();
