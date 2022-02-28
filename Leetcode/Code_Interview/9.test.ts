(function () {
  // Version 3; Small difference
  // with Version 1 in `deleteHead()` method.

  class CQueue {
    stack1: number[];
    stack2: number[];
    constructor() {
      this.stack1 = []; // Enqueue into this stack.
      this.stack2 = []; // Dequeue from this stack.
    }

    appendTail(value: number): void {
      this.stack1.push(value);
    }

    deleteHead(): number {
      const len2 = this.stack2.length;

      if (len2) {
        return this.stack2.pop()!;
      }

      // Pop all elements in stack1 and push them into stack2 reversely.
      while (this.stack1.length) {
        this.stack2.push(this.stack1.pop()!);
      }

      return this.stack2.length ? this.stack2.pop()! : -1;
    }
  }
})();
(function () {
  // Version 2
  class CQueue {
    stack1: number[];
    stack2: number[];
    constructor() {
      this.stack1 = []; // Enqueue into this stack.
      this.stack2 = []; // Dequeue from this stack.
    }

    appendTail(value: number): void {
      this.stack1.push(value);
    }

    deleteHead(): number {
      const len1 = this.stack1.length;
      const len2 = this.stack2.length;

      if (len2) {
        return this.stack2.pop()!;
      }

      if (len1) {
        // Pop all elements in stack1 and push them into stack2 reversely.
        while (this.stack1.length) {
          this.stack2.push(this.stack1.pop()!);
        }

        return this.stack2.pop()!;
      }

      return -1;
    }
  }
})();

(function () {
  // Version 1
  // The below isn't a proper solution
  // due to it use array instead of two stacks.
  class CQueue {
    arr: number[];
    constructor() {
      this.arr = [];
    }

    appendTail(value: number): void {
      this.arr.push(value);
    }

    deleteHead(): number {
      if (this.arr.length > 0) {
        // Why does need non-null assertion?
        // Because the return type of the `shift()` function signature.
        return this.arr.shift()!;
      }

      return -1;
    }
  }

  /**
   * Your CQueue object will be instantiated and called as such:
   * var obj = new CQueue()
   * obj.appendTail(value)
   * var param_2 = obj.deleteHead()
   */
})();
