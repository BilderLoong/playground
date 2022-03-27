let line;
while ((line = read_line()) != '') {
  // Your code
  let res = -1;
  const arr = line.split('');
  const map = new Map();
  arr.forEach((v) => {
    if (map.has(v)) {
      map.set(v, false);
    } else {
      map.set(v, true);
    }
  });

  for (let i = 0; i < arr.length; i++) {
    const e = arr[i];
    if (map.get(e)) {
      res = i;
      break;
    }
  }

  console.log(res);
}
