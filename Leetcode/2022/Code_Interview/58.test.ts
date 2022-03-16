describe('test', () => {
  // Version 2
  test('should first', () => {
    expect(reverseLeftWords('abcdefg', 2)).toBe('cdefgab');
    expect(reverseLeftWords('lrloseumgh', 6)).toBe('umghlrlose');
  });
  function reverseLeftWords(s: string, n: number): string {
    // const stringToMove = s.slice(0, n);
    // const stringUntouched = s.slice(n);
    // return stringUntouched + stringToMove
    return s.slice(n) + s.slice(0, n);
  }
});

describe('test', () => {
  // Version 1
  test('should first', () => {
    expect(reverseLeftWords('abcdefg', 2)).toBe('cdefgab');
    expect(reverseLeftWords('lrloseumgh', 6)).toBe('umghlrlose');
  });

  function reverseLeftWords(s: string, n: number): string {
    const arr = s.split('');
    const arrToMove = arr.splice(0, n);
    return arr.concat(arrToMove).join('');
  }
});
