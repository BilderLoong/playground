export {};
const isArr = (val: any) => {
  const m1 = Array.isArray(val);
  const m2 = typeof val === 'object' && val?.constructor === Array;
  const m3 = val instanceof Array;
  const m4 = Object.prototype.toString.call(val) === '[object Array]';

  return m1 && m2 && m3 && m4;
};

function isContainArr(arr: Array<any>): boolean {
  return arr.some((v) => isArr(v));
}

it('should ', () => {
  expect(flatten0([1, 1, 1, [3, 3], []])).toEqual([1, 1, 1, 3, 3]);
  expect(flatten1([1, 1, 1, [3, 3], []])).toEqual([1, 1, 1, 3, 3]);
  expect(flatten2([1, 1, 1, [3, 3], []])).toEqual([1, 1, 1, 3, 3]);
  expect(flatten3([1, 1, 1, [3, 3], []])).toEqual([1, 1, 1, 3, 3]);
  expect(flatten4([1, 1, 1, [3, 3], []])).toEqual([1, 1, 1, 3, 3]);
  function flatten4(arr: any[]): any[] | void {
    // Reduce

    return arr.reduce(
      // (pre, cur) => (isArr(cur) ? pre.concat(flatten4(cur)) : pre.concat(cur)),
      (pre, cur) => pre.concat(isArr(cur) ? flatten4(cur) : cur),
      []
    );
  }

  function flatten3(arr: any[]): any[] | void {
    // Recursion
    let res: any[] = [];

    arr.forEach((v) => {
      if (isArr(v)) {
        res = res.concat(flatten3(v));
      } else {
        res.push(v);
      }
    });

    return res;
  }

  function flatten2(arr: any[]): any[] {
    if (!isContainArr(arr)) {
      return arr;
    }

    return flatten2(([] as any[]).concat(...arr));
  }

  function flatten0(arr: any[]) {
    while (isContainArr(arr)) {
      arr = [].concat(...arr);
    }

    return arr;
  }

  function flatten1(arr: any[]) {
    const res = [];

    while (arr.length) {
      const top = arr.pop();

      if (isArr(top)) {
        arr.push(...top);
      } else {
        res.unshift(top);
      }
    }

    return res;
  }
});
