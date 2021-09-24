/**
 * @param {ListNode} head
 * @return {boolean}
 * convert the linked list to an array
 */

var isPalindrome = function (head) {
  const arr = [];
  while (head) {
    arr.push(head.val);
    head = head.next;
  }

  for (let i = 0, j = arr.length - 1; i < j; i++, j--) {
    if (arr[i] !== arr[j]) return false;
  }
  return true;
};

//----------------------------------------------------------------

/* using post order traverse linked list */
var isPalindrome = function (head) {
  let left = head;
  return traverse(head);

  function traverse(head) {
    if (head === null) return true;
    let result = traverse(head.next);
    result = result && left.val === head.val;
    left = left.next;

    return result;
  }
};

//---------------------------------------------------

// function ListNode(val, next) {
//   this.val = val === undefined ? 0 : val;
//   this.next = next === undefined ? null : next;
// }

// let a = new ListNode(1)
// let b = new ListNode(2)
// a.next = b;
// isPalindrome(a)

function isPalindrome(head) {
  let fast, slow;
  fast = slow = head;

  while (fast !== null && fast.next !== null) {
    fast = fast.next.next;
    slow = slow.next;
  }

  slow = fast !== null ? slow.next : slow;

  const newHead = reverseList(slow);
  let right = newHead;
  let left = head;

  while (right) {
    if (left.val !== right.val) return false;

    right = right.next;
    left = left.next;
  }

  return true;

  function reverseList(head) {
    if (head === null || head.next === null) return head;

    const newHead = reverseList(head.next);
    head.next.next = head;
    head.next = null;

    return newHead;
  }
}

//---------------------------------------------------

/* just like above, except reverse the left part of the list
   during the slow pointer move */
function isPalindrome(head) {
  let fast,
    slow,
    slowPre = null;
  fast = slow = head;

  while (fast !== null && fast.next !== null) {
    fast = fast.next.next;

    const next = slow.next;
    slow.next = slowPre;
    slowPre = slow;
    slow = next;
  }

  /* If the slow pointer is in the middle of the linked list
    (In other word,the number of the linked is odd.) we need move the 
    slow pointer to the next so that the slow pointer is located 
    at the beginning of the right half linked list.  */

  slow = fast !== null ? slow.next : slow;

  while (slow) {
    if (slow.val !== slowPre.val) return false;
    slow = slow.next;
    slowPre = slowPre.next;
  }

  return true;
}
