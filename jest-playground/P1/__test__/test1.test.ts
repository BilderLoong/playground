export const reduceAvg = ({ preAvg, cur, index }: Record<string, number>) => {
  if (index === 0) {
    return cur;
  }

  const preSum = preAvg * index;
  const curBase = index + 1;

  return (preSum + cur) / curBase;
};

describe('reduceAvg', () => {
  it('should return right avg when index equal to 0', () => {
    const array = [1];

    const res = array.reduce((pre, cur, index) =>
      reduceAvg({ preAvg: pre, cur, index })
    );

    expect(res).toEqual(1);
  });

  it('should return right avg when index equal to 0', () => {
    const array: number[] = [];

    const res = array.reduce(
      (pre, cur, index) => reduceAvg({ preAvg: pre, cur, index }),
      0
    );

    expect(res).toEqual(0);
  });
  it('should return right avg when index equal to 0', () => {
    const array = [1, 2];

    const res = array.reduce((pre, cur, index) =>
      reduceAvg({ preAvg: pre, cur, index })
    );

    expect(res).toEqual(1.5);
  });
});
