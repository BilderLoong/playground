export {};
(function () {
  // solution2
  // better time complexity
  class CQueue {
    stack1: number[] = []; // enqueue to this stack
    stack2: number[] = []; // dequeue from this stack

    appendTail(value: number): void {
      this.stack1.push(value);
    }

    deleteHead(): number {
      let res = -1;
      if (this.stack2.length) {
        res = this.stack2.pop()!;
      } else if (this.stack1.length) {
        this.stack2 = CQueue.reverse(this.stack1);

        res = this.stack2.pop()!;
      }

      return res;
    }

    // return the reversed input array, and empty the input array
    static reverse(arr: number[]): number[] {
      const res: number[] = [];
      while (arr.length) {
        res.push(arr.pop()!);
      }

      return res;
    }
  }

  //test ---------------------
  const que = new CQueue();

  //-------------appendTail test-------------------

  que.appendTail(1);
  console.assert(
    JSON.stringify(que.stack1) === JSON.stringify([1]),
    que.stack1
  );

  que.appendTail(2);
  console.assert(
    JSON.stringify(que.stack1) === JSON.stringify([1, 2]),
    que.stack1
  );

  que.deleteHead();

  console.assert(JSON.stringify(que.stack1) === JSON.stringify([]), que.stack1);
  console.assert(
    JSON.stringify(que.stack2) === JSON.stringify([2]),
    que.stack2
  );

  //------------
  que.deleteHead();

  console.assert(JSON.stringify(que.stack1) === JSON.stringify([]), que.stack1);
  console.assert(JSON.stringify(que.stack2) === JSON.stringify([]), que.stack2);
  //----
  console.assert(que.deleteHead() === -1);

  console.assert(JSON.stringify(que.stack1) === JSON.stringify([]), que.stack1);
  console.assert(JSON.stringify(que.stack2) === JSON.stringify([]), que.stack2);

  // reverse test==================================================
  const reverseTest = [1, 2, 3];
  const res = CQueue.reverse(reverseTest);
  console.assert(JSON.stringify(res) === JSON.stringify([3, 2, 1]));
  console.assert(JSON.stringify(reverseTest) === JSON.stringify([]));

  /**
   * Your CQueue object will be instantiated and called as such:
   * var obj = new CQueue()
   * obj.appendTail(value)
   * var param_2 = obj.deleteHead()
   */
})();
(function () {
  // solution1 brute force
  class CQueue {
    // keep the below two stack reverse same
    stack1: number[] = []; // append to this stack
    stack2: number[] = []; // delete from this stack

    appendTail(value: number): void {
      this.stack1.push(value);
    }

    deleteHead(): number {
      if (!this.stack1.length) return -1;

      // no need to refresh the stack2 every time deleteHead()
      // detail on the solution 2
      this.stack2 = CQueue.reverse(this.stack1);
      const res = this.stack2.pop()!;

      // no need to refresh the stack1 every time deleteHead()
      this.stack1 = CQueue.reverse(this.stack2);

      return res;
    }

    // return the reversed input array
    static reverse(arr: number[]): number[] {
      return [...arr].reverse();
    }
  }

  //test ---------------------
  const que = new CQueue();

  //-------------appendTail test-------------------

  que.appendTail(1);
  console.assert(
    JSON.stringify(que.stack1) === JSON.stringify([1]),
    que.stack1
  );

  que.appendTail(2);
  console.assert(
    JSON.stringify(que.stack1) === JSON.stringify([1, 2]),
    que.stack1
  );

  que.deleteHead();

  console.assert(
    JSON.stringify(que.stack1) === JSON.stringify([2]),
    que.stack1
  );
  console.assert(
    JSON.stringify(que.stack2) === JSON.stringify([2]),
    que.stack2
  );

  // reverse test==================================================
  const reverseTest = [1, 2, 3];
  const res = CQueue.reverse(reverseTest);
  console.assert(JSON.stringify(res) === JSON.stringify([3, 2, 1]));

  /**
   * Your CQueue object will be instantiated and called as such:
   * var obj = new CQueue()
   * obj.appendTail(value)
   * var param_2 = obj.deleteHead()
   */
})();
