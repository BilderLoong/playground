import LinkedListNode from './Linked-List-Node';

export default class LinkedList {
  head: LinkedListNode | null;
  tail: LinkedListNode | null;

  constructor() {
    this.head = null;
    this.tail = null;
  }

  prepend(value: unknown) {
    const node = new LinkedListNode(value);
    if (this.head) {
      node.next = this.head;
      this.head = node;
    } else {
      this.head = this.tail = node;
    }

    //----------------method2------------------
    // const node = new LinkedListNode(value,this.head);
    // this.head = node;

    // if (!this.tail) {
    //   this.tail = node;
    // }
    //---------------------------------------
    return this;
  }

  append(value: unknown) {
    const node = new LinkedListNode(value);
    if (this.tail) {
      this.tail.next = node;
      this.tail = node;
    } else {
      this.head = this.tail = node;
    }

    return this;
  }

  // delete all nodes whose value is same to the given value
  // and return the reference of last one be deleted
  // if no correspond value is found return null
  delete(value: unknown): LinkedListNode | null {
    if (!this.head) {
      return null;
    }

    let deletedNode = null;

    // if the head need to be delete
    while (this.head && this.head.value === value) {
      deletedNode = this.head;
      this.head = this.head.next;
    }

    let curNode = this.head;

    if (curNode) {
      while (curNode.next) {
        if (curNode.next.value === value) {
          deletedNode = curNode.next;
          curNode.next = curNode.next.next;
        } else {
          curNode = curNode.next;
        }
      }
    }

    // if the tail is be delete, need update
    // this.tail to the new tail which is curNode
    if (this.tail!.value === value) {
      this.tail = curNode;
    }

    return deletedNode;
  }

  // delete the first node whose value is same to
  // the given value and return the deleted node
  // if no value is found return null
  // e.g.:1->2->3->2->null
  // after deleteOne(2): 1->3->2->null
  deleteOne(value: unknown): LinkedListNode | null {
    if (!this.head) return null;

    let deletedNode = null;

    // if the head need to be delete
    if (this.head.value === value) {
      deletedNode = this.head;
      this.head = this.head.next;

      return deletedNode;
    }

    let curNode = this.head;

    if (curNode) {
      while (curNode.next !== null) {
        if (curNode.next.value === value) {
          deletedNode = curNode.next;
          curNode.next = curNode.next.next;
          break;
        } else {
          curNode = curNode.next;
        }
      }
    }

    // if the tail is be delete, need update
    // this.tail to the new tail which is curNode
    if (!curNode.next) {
      this.tail = curNode;
    }

    return deletedNode;
  }

  // find the given value
  // return the node first be found, if not found return null
  find(value: unknown): LinkedListNode | null {
    if (!this.head) return null;

    let curNode: LinkedListNode | null = this.head;
    while (curNode) {
      if (value === curNode.value) return curNode;
      else curNode = curNode.next;
    }

    return null;
  }

  // return the deleted tail
  // if no tail return null
  deleteTail(): LinkedListNode | null {
    const deletedTail = this.tail;

    if (this.head === this.tail) {
      this.head = this.tail = null;
      return deletedTail;
    }

    let curNode: LinkedListNode = this.head!;

    while (curNode.next !== this.tail) {
      curNode = curNode.next!;
    }

    // delete the tail
    curNode.next = null;
    this.tail = curNode;

    return deletedTail;
  }

  deleteHead(): LinkedListNode | null {
    if (!this.head) return null;

    let deletedHead = this.head;

    if (this.head === this.tail) {
      this.head = this.tail = null;
    } else {
      this.head = this.head.next;
    }

    return deletedHead;
  }

  // append a arr to current list
  appendFromArray(arr: any[]) {
    arr.forEach((e) => {
      this.append(e);
    });

    return this;
  }

  toArray(): LinkedListNode[] {
    const nodes = [];
    let node = this.head;

    while (node) {
      nodes.push(node);
      node = node.next;
    }

    return nodes;
  }

  toString(callback?: Function) {
    return this.toArray()
      .map((node: LinkedListNode) => node.toString(callback))
      .toString();
  }

  // return the head of reversed list
  reverse(): LinkedList | null {
    if (!this.head) return this.head;

    let pre = null;
    let cur: LinkedListNode | null = this.head;

    while (cur) {
      const next: LinkedListNode | null = cur.next;
      cur.next = pre;
      pre = cur;
      cur = next;
    }

    this.tail = this.head;
    this.head = pre;
    return this;
  }
}
