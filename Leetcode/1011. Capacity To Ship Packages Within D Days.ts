(function () {
  // left binary search

  const test4 = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
  const res4 = shipWithinDays(test4, 5);
  console.assert(res4 === 15, 4);

  const test5 = [3, 2, 2, 4, 1, 4];
  const res5 = shipWithinDays(test5, 3);
  console.assert(res5 === 6, 5);

  function shipWithinDays(weights: number[], D: number): number {
    let low = Math.max(...weights);
    let high = weights.reduce((p, c) => p + c) + 1; //

    while (low < high) {
      const mid = Math.floor((high + low) / 2);

      if (canShipAll(weights, D, mid)) {
        high = mid;
      } else {
        low = mid + 1;
      }
    }

    return low;
  }
})();
(function () {
  // Brute force

  const test4 = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
  const res4 = shipWithinDays(test4, 5);
  console.assert(res4 === 15, 4);

  const test5 = [3, 2, 2, 4, 1, 4];
  const res5 = shipWithinDays(test5, 3);
  console.assert(res5 === 6, 5);

  function shipWithinDays(weights: number[], D: number): number {
    for (let i = Math.max(...weights); ; i++) {
      if (canShipAll(weights, D, i)) {
        return i;
      }
    }
  }

  //==================================================

  const test1 = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
  const res1 = canShipAll(test1, 5, 10);
  console.assert(res1 == false, '1');

  const test2 = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
  const res2 = canShipAll(test2, 5, 15);
  console.assert(res2 == true, '2');

  const test3 = [6, 5, 1];
  const res3 = canShipAll(test3, 2, 6);
  console.assert(res3 == true, '3');
})();

//==================================================

const test1 = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
const res1 = canShipAll(test1, 5, 10);
console.assert(res1 == false, '1');

const test2 = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
const res2 = canShipAll(test2, 5, 15);
console.assert(res2 == true, '2');

const test3 = [6, 5, 1];
const res3 = canShipAll(test3, 2, 6);
console.assert(res3 == true, '3');

function canShipAll(weights: number[], D: number, capacity: number): boolean {
  let weightOfCurrentDay = 0;

  // Need set the init value to 1 instead of 0,
  // consider when weights === [5, 1, 2], capacity === 5
  // if the init value of days is 0,  then at the end of loop,
  // the value of `days` is not added up the day to ship [1, 2]
  let days = 1;

  for (let i = 0; i < weights.length; i++) {
    const e = weights[i];
    if (weightOfCurrentDay + e <= capacity) {
      weightOfCurrentDay += e;
    } else {
      days++;
      weightOfCurrentDay = e;

      if (days > D) return false;
    }
  }

  return true;
}

//==========================
(function () {
  //howManyDays version2

  const test1 = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
  const res1 = howManyDays(test1, 10);
  console.assert(res1 == 7, `howMayDays.v2:1 ${res1}`);

  const test2 = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
  const res2 = howManyDays(test2, 15);
  console.assert(res2 == 5, `howMayDays.v2:2 ${res2}`);

  const test3 = [6, 5, 1];
  const res3 = howManyDays(test3, 6);
  console.assert(res3 == 2, `howMayDays.v2:3 ${res3}`);

  const test4 = [6, 5, 1];
  const res4 = howManyDays(test4, 6);
  console.assert(res4 == 2, `howMayDays.v2:4 ${res4}`);

  // return the number of the days needed
  // more concise compare to the logical in the `canShipAll`
  function howManyDays(weights: number[], capacity: number): number {
    let weightOfCurrentDay = 0;

    // because the days will plus one at the end of the
    // outer loop, so there are need to set the days equal to 0.
    let days = 0;

    for (let i = 0; i < weights.length; ) {
      while (
        i < weights.length &&
        weightOfCurrentDay + weights[i] <= capacity
      ) {
        weightOfCurrentDay += weights[i];
        i++;
      }

      weightOfCurrentDay = 0;
      days++;
    }

    return days;
  }
})();

(function () {
  //==================================================

  const test1 = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
  const res1 = howManyDays(test1, 10);
  console.assert(res1 == 7, '1');

  const test2 = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
  const res2 = howManyDays(test2, 15);
  console.assert(res2 == 5, '2');

  const test3 = [6, 5, 1];
  const res3 = howManyDays(test3, 6);
  console.assert(res3 == 2, '3');

  // return the number of the days needed
  // more concise compare to the logical in the `canShipAll`
  function howManyDays(weights: number[], capacity: number): number {
    let weightOfCurrentDay = 0;

    // Need set the init value to 1 instead of 0,
    // consider when weights === [5, 1, 2], capacity === 5
    // if the init value of days is 0,  then at the end of loop,
    // the value of `days` is not added up the day to ship [1, 2]
    let days = 1;

    for (const weight of weights) {
      if (weightOfCurrentDay + weight > capacity) {
        days++;
        weightOfCurrentDay = 0;
      }

      weightOfCurrentDay += weight;
    }

    return days;
  }
})();
