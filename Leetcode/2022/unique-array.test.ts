it('should ', () => {
  const arr = ['A', 'B', 'A', 'C', 'B'];
  expect(unique(arr)).toEqual(['A', 'B', 'C']);

  function unique(arr: any[]): any[] {
    // In-place
    for (let i = 0; i < arr.length; i++) {
      if (arr.indexOf(arr[i]) !== i) {
        arr.splice(i, 1);
      }
    }

    return arr;
  }
});

it('should ', () => {
  const arr = ['A', 'B', 'A', 'C', 'B'];
  expect(unique(arr)).toEqual(['A', 'B', 'C']);

  function unique(arr: any[]): any[] {
    return arr.reduce((pre, cur) => {
      if (pre.includes(cur)) {
        return pre;
      }
      pre.push(cur);
      return pre;
    }, []);
  }
});

it('should ', () => {
  const arr = ['A', 'B', 'A', 'C', 'B'];
  expect(unique(arr)).toEqual(['A', 'B', 'C']);

  function unique(arr: any[]): any[] {
    return arr.filter((v, i) => arr.indexOf(v) === i);
  }
});

it('should ', () => {
  const arr = ['A', 'B', 'A', 'C', 'B'];
  expect(unique(arr)).toEqual(['A', 'B', 'C']);

  function unique(arr: any[]): any[] {
    const set = new Set();
    return arr.filter((v) => {
      if (!set.has(v)) {
        set.add(v);
        return true;
      } else {
        return false;
      }
    });
  }
});

it('should ', () => {
  const arr = ['A', 'B', 'A', 'C', 'B'];
  expect(unique(arr)).toEqual(['A', 'B', 'C']);

  function unique(arr: any[]): any[] {
    return Array.from(new Set(arr));
  }
});

it('should ', () => {
  const arr = ['A', 'B', 'A', 'C', 'B'];
  expect(unique(arr)).toEqual(['A', 'B', 'C']);

  function unique(arr: any[]): any[] {
    return [...new Set(arr)];
  }
});
