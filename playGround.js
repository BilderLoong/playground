// function ListNode(val, next) {
//   this.val = val === undefined ? 0 : val;
//   this.next = next === undefined ? null : next;
// }

// const first = new ListNode(3);
// const second = new ListNode(5);

// first.next = second;

// reverseBetween(first, 1, 2);

function reverseBetween(head, left, right) {
  const dummyNode = new ListNode(-1, head);

  let pre = dummyNode;

  for (let i = 0; i < left - 1; i++) {
    // edge case may be wrong.
    pre = pre.next;
  }

  const theOneBeforeLeft = pre;

  const leftNode = (pre = theOneBeforeLeft.next); // left node
  theOneBeforeLeft.next = null; // avoid cycle

  let cur = pre.next;

  for (let i = left; i < right; i++) {
    const next = cur.next;
    cur.next = pre;
    pre = cur;
    cur = next;
  }

  const rightNode = pre;
  const theOneAfterRight = cur;

  theOneBeforeLeft.next = rightNode;
  leftNode.next = theOneAfterRight;

  dummyNode.next = null;

  // if left === 1 then the head of the origin list will be changed to
  // the right node
  return left === 1 ? rightNode : head;
}
