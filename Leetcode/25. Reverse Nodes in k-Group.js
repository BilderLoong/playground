/**
 * Definition for singly-linked list.
 * function ListNode(val, next) {
 *     this.val = (val===undefined ? 0 : val)
 *     this.next = (next===undefined ? null : next)
 * }
 */
/**
 * @param {ListNode} head
 * @param {number} k
 * @return {ListNode}
 */
var reverseKGroup = function (head, k) {
  let startOfNextRecursion = head;
  for (let i = 0; i < k; ++i) {
    if (startOfNextRecursion === null) return head;
    startOfNextRecursion = startOfNextRecursion.next;
  }

  const newHead = reverse(head, k);
  head.next = reverseKGroup(startOfNextRecursion, k);

  return newHead;
};

/** 
 @return {ListNode}
 reverse first nums node
*/
function reverse(head, nums) {
  let pre = head;
  let cur = pre.next;

  for (let i = 1; i < nums; i++) {
    const next = cur.next;
    cur.next = pre;
    pre = cur;
    cur = next;
  }

  // pre is the new head
  return pre;
}

function reverseBetween(head, left, right) {
  const dummyNode = new ListNode(-1, head);

  let pre = dummyNode;

  for (let i = 0; i < left - 1; i++) {
    // edge case may be wrong.
    pre = pre.next;
  }

  const theOneBeforeLeft = pre;

  const leftNode = (pre = theOneBeforeLeft.next); // Left node
  theOneBeforeLeft.next = null; // Avoid cycle

  let cur = pre.next;

  for (let i = left; i < right; i++) {
    const next = cur.next;
    cur.next = pre;
    pre = cur;
    cur = next;
  }

  // At this point pre is the right node
  theOneBeforeLeft.next = pre;

  // At this point cur is the one after right node
  leftNode.next = cur;

  /* If don't use dummyNOde need to consider the case 
  when the left === 1(the head of the linked list is changed to the right node)
  by using the below code:
    return left === 1 ? pre : head; */
  return dummyNode.next;
}
