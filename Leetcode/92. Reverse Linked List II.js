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

  let theOneBeforeLeft = dummyNode;

  for (let i = 0; i < left - 1; i++) {
    // edge case may be wrong.
    theOneBeforeLeft = theOneBeforeLeft.next;
  }

  let cur = theOneBeforeLeft.next;

  for (let i = 0; i < right - left; i++) {
    const next = cur.next;
    cur.next = next.next;
    next.next = theOneBeforeLeft.next;
    theOneBeforeLeft.next = next;
  }

  /* return dummyNode.next instead of head is when the left === 1,
   the head of old link list is not the head of new link list  */
  return dummyNode.next;
}

/* using different method from the above*/
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
