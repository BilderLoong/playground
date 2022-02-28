export default class LinkedListNode {
  value: unknown;
  next: LinkedListNode | null;

  constructor(val: unknown, next: LinkedListNode | null = null) {
    this.value = val;
    this.next = next;
  }

  toString(callback?: Function): string {
    return callback ? callback(this.value) : `${this.value}`;
  }
}
