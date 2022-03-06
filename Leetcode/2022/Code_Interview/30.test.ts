class MinStack {
  mainStack: number[] = [];
  auxiliaryStack: number[] = [];
  constructor() {}

  push(x: number): void {
    this.mainStack.push(x);

    if (x <= this.min()) {
      this.auxiliaryStack.push(x);
    }
  }

  pop(): void {
    if (this.top() === this.min()) {
      this.auxiliaryStack.pop();
    }

    this.mainStack.pop();
  }

  top(): number {
    return this.mainStack[this.mainStack.length - 1];
  }

  min(): number {
    const auxiliaryStackLen = this.auxiliaryStack.length;
    const min = this.auxiliaryStack[auxiliaryStackLen - 1] ?? Infinity;
    return min;
  }
}

/**
 * Your MinStack object will be instantiated and called as such:
 * var obj = new MinStack()
 * obj.push(x)
 * obj.pop()
 * var param_3 = obj.top()
 * var param_4 = obj.min()
 */
