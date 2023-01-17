const sqrt = (x: number) => {
  let g = 1;
  let preG = 0;
  let dealt = Math.abs(g - preG);
  while (dealt > 0.00000000000001) {
    preG = g;
    g -= (g * g - x) / (2 * x);
    dealt = Math.abs(preG - g);
  }

  return g;
};

console.log(sqrt(4));
console.log(sqrt(3));
