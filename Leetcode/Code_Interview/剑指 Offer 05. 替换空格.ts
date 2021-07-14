(function () {
  console.assert(replaceSpace('s s s') === 's%20s%20s', replaceSpace('s s s'));

  function replaceSpace(s: string): string {
    const oldLen = s.length;
    let numberOfSpace = 0;
    for (const i of s) {
      if (i.match(/\s/)) {
        numberOfSpace++;
      }
    }
    const newLen = oldLen + numberOfSpace * 2;

    const arr = s.split('');
    for (let i = oldLen - 1, j = newLen - 1; i < j; i--, j--) {
      if (arr[i] !== ' ') arr[j] = arr[i];
      else {
        arr[j--] = '0';
        arr[j--] = '2';
        arr[j] = '%';
      }
    }

    return arr.join('');
  }
})();
(function () {
  console.assert(replaceSpace('s s s') === 's%20s%20s');

  function replaceSpace(s: string): string {
    const arr = [];
    for (const i of s) {
      if (i.match(/\s/)) {
        arr.push('%20');
      } else {
        arr.push(i);
      }
    }
    return arr.join('');
  }
})();
(function () {
  function replaceSpace(s: string): string {
    const arr = Array(s.length * 3);
    let size = 0;
    for (let i = 0; i < s.length; i++) {
      const e = s[i];
      if (e === ' ') {
        [arr[size], arr[size + 1], arr[size + 2]] = ['%', '2', '0'];
        size += 3;
      } else {
        arr[size] = e;
        size++;
      }
    }
    return arr.join('');
  }

  console.log(replaceSpace('s s s'));
})();

(function () {
  // RegEX
  function replaceSpace(s: string): string {
    return s.replace(/\s/g, '%20');
  }

  console.log(replaceSpace('s s s'));
})();

(function () {
  //O(n^2)
  function replaceSpace(s: string): string {
    const arr = Array.from(s);
    arr.forEach((e, i) => {
      if (e === ' ') {
        arr.splice(i, 1, '%20');
      }
    });

    return arr.join('');
  }

  console.log(replaceSpace('s s s'));
})();
