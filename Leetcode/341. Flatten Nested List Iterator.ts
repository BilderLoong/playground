// This is the interface that allows for creating nested lists.
// You should not implement it, or speculate about its implementation

class NestedInteger {
  val: null | number;
  list: null | NestedInteger;
  //  If value is provided, then it holds a single integer
  //  Otherwise it holds an empty nested list
  constructor(value?: number) {
    this.val = value ?? null;
    this.list = null;
  }

  //  Return true if this NestedInteger holds a single integer, rather than a nested list.
  isInteger(): boolean {
    return this.val !== null;
  }

  //  Return the single integer that this NestedInteger holds, if it holds a single integer
  //  Return null if this NestedInteger holds a nested list
  getInteger(): number | null {
    return this.val;
  }

  //  Set this NestedInteger to hold a single integer equal to value.
  setInteger(value: number) {
    this.val = value;
  }

  //  Set this NestedInteger to hold a nested list and adds a nested integer elem to it.
  add(elem: NestedInteger) {
    this.list = elem;
  }

  //  Return the nested list that this NestedInteger holds,
  //  or an empty list if this NestedInteger holds a single integer
  getList(): NestedInteger[] {}
}

/* using a stack to implement lazy iterator(only genator value when needed)
class NestedIterator {
  stack: NestedInteger[];
  constructor(nestedList: NestedInteger[]) {
    this.stack = nestedList;
  }

  hasNext(): boolean {
    while (this.stack.length) {
      if (this.stack[0].isInteger()) {
        return true;
      } else {
        const list = this.stack.shift();
        this.stack.unshift(...list!.getList());
      }
    }
  }

  next(): number {
    return this.stack.shift()!.getInteger()!;
  }
}

class NestedIterator {
  vals: number[];
  list: NestedInteger[];
  constructor(nestedList: NestedInteger[]) {
    this.vals = [];
    this.list = nestedList;

    /* using one of the below two function*/
    // this.dfs(this.list);
    // this.vals = this.dfsPureFunc(this.list);
  // }

  dfsPureFunc(nestIntegers: NestedInteger[]): number[] {
    /* don't have side effect */
    const integers: number[] = [];

    for (const nestInteger of nestIntegers) {
      if (nestInteger.isInteger()) integers.push(nestInteger.getInteger()!);
      else {
        integers.push(...this.dfsPureFunc(nestInteger.getList()));
      }
    }

    return integers;
  }

  dfs(nestIntegers: NestedInteger[]): void {
    for (const nestInteger of nestIntegers) {
      if (nestInteger.isInteger()) this.vals.push(nestInteger.getInteger()!);
      else {
        this.dfs(nestInteger.getList());
      }
    }
  }

  hasNext(): boolean {
    return this.vals.length > 0;
  }

  next(): number {
    const val = this.vals.shift();
    return val!;
  }
}

/**
 * Your ParkingSystem object will be instantiated and called as such:
 * var obj = new NestedIterator(nestedList)
 * var a: number[] = []
 * while (obj.hasNext()) a.push(obj.next());
 */
