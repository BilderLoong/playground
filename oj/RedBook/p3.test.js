const isItemOnly = (arr) => {
  const dupSet = new Set();
  const uniSet = new Set();

  arr.forEach((v) => {
    if (!dupSet.has(v)) {
      if (uniSet.has(v)) {
        uniSet.delete(v);
        dupSet.add(v);
      } else {
        uniSet.add(v);
      }
    }
  });

  return uniSet.size === 1;
};

let line;
while ((line = read_line()) != '') {
  const arr = JSON.parse(line);
  console.log(isItemOnly(arr));
}

it('should ', () => {
  expect(isItemOnly([1, 1, 2, 2, 0, 0])).toBe(false);
  expect(isItemOnly([1, 1, 2, 2, 0, 3])).toBe(false);
  expect(isItemOnly([])).toBe(false);
  expect(isItemOnly([1, 1, 2, 2, 0])).toBe(true);
  expect(isItemOnly([0])).toBe(true);
});

// 判断数组中元素有没有只出现一次未重复的情况

(function () {
  const isItemOnly = (arr) => {
    // Your code
    let count = 0;
    const map = new Map();

    arr.forEach((v) => {
      if (map.has(v)) {
        map.set(v, map.get(v) + 1);
      } else {
        map.set(v, 1);
      }
    });

    map.forEach((v) => {
      if (v === 1) {
        count++;

        if (count > 1) {
          return false;
        }
      }
    });

    return count === 1;
  };
})();
