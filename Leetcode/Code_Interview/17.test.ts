try {
} catch (error) {}
try {
  it('test2', () => {
    expect(printNumbers(2)).toEqual(printNumbersRight(2));
  });
  it('test1', () => {
    expect(printNumbers(1)).toEqual(printNumbersRight(1));
  });

  // Consider number overflow
  function printNumbers(n: number): Array<number | string> {
    const res: string[] = [];

    for (let m = 1; m <= n; m++) {
      for (let i = 1; i <= 9; i++) {
        dfs(i.toString(), m);
      }
    }

    return res;

    function dfs(curStr: string, lengthOfNumber: number) {
      if (curStr.length === lengthOfNumber) {
        res.push(curStr);
        return;
      }

      for (let i = 0; i <= 9; i++) {
        dfs(curStr.concat(i.toString()), lengthOfNumber);
      }
    }
  }
  // solution 4: DFS
  var printNumbersRight = function (n: number) {
    let res: Array<string> = [];
    var dfs = function (stringOfNumber: string, lengthOfBit: number) {
      if (stringOfNumber.length == lengthOfBit) {
        res.push(stringOfNumber);
        return;
      }
      for (let i = 0; i <= 9; i++) {
        stringOfNumber += i.toString();
        dfs(stringOfNumber, lengthOfBit);
        // remove the last digit after using it
        stringOfNumber = stringOfNumber.slice(0, -1);
      }
    };
    for (let m = 1; m <= n; m++) {
      for (let i = 1; i <= 9; i++) {
        dfs(i.toString(), m);
      }
    }
    return res;
  };
} catch (error) {}

try {
  // Don't consider number overflow;
  function printNumbers(n: number): number[] {
    const res = [];
    for (let i = 1; i < 10 ** n; i++) {
      res.push(i);
    }

    return res;
  }
} catch (error) {}
