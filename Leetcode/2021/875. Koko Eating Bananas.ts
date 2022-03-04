(function () {
  // binary searching
  function minEatingSpeed(piles: number[], h: number): number {
    let left = 1;
    let right = Math.max(...piles);

    while (left < right) {
      const mid = Math.floor((left + right) / 2);
      if (canEatAll(mid, piles, h)) {
        // [left...mid)
        right = mid;
      } else {
        // [mid+1...right)
        left = mid + 1;
      }
    }

    return left;
  }

  //-----------------test for minEatingSpeed----------------------

  const res1 = minEatingSpeed([3, 6, 7, 11], 8);
  console.assert(res1 === 4, `minEatingSpeed test:${res1}`);

  const res2 = minEatingSpeed([30, 11, 23, 4, 20], 5);
  console.assert(res2 === 30, `minEatingSpeed test:${res2}`);
})();

(function () {
  //liner searching timeout

  //-----------------test for minEatingSpeed----------------------

  const res1 = minEatingSpeed([3, 6, 7, 11], 8);
  console.assert(res1 === 4, `minEatingSpeed test:${res1}`);

  const res2 = minEatingSpeed([30, 11, 23, 4, 20], 5);
  console.assert(res2 === 30, `minEatingSpeed test:${res2}`);

  function minEatingSpeed(piles: number[], h: number): number {
    const maxSpeed = Math.max(...piles);

    for (let i = 1; i <= maxSpeed; i++) {
      if (canEatAll(i, piles, h)) return i;
    }
  }
})();

//---------------test for canEatAll----------------------

console.assert(canEatAll(1, [3, 6, 7, 11], 8) === false, 1);
console.assert(canEatAll(2, [3, 6, 7, 11], 8) === false, 2);
console.assert(canEatAll(3, [3, 6, 7, 11], 8) === false, 3);

console.assert(canEatAll(4, [3, 6, 7, 11], 8) === true, 4);
console.assert(canEatAll(5, [3, 6, 7, 11], 8) === true, 5);

function canEatAll(speed: number, piles: number[], h: number): boolean {
  let timeCost = 0;

  piles.forEach((e) => {
    const reminder = e % speed > 0 ? 1 : 0;
    timeCost += Math.floor(e / speed) + reminder;
  });

  return h >= timeCost;
}
