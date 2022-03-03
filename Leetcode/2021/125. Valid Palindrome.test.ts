it('test3', () => {
  const s = 'OP';
  expect(isPalindrome(s)).toBe(false);
});

it('test1', () => {
  const s = 'A man, a plan, a canal: Panama';
  expect(isPalindrome(s)).toBe(true);
});

it('test2', () => {
  const s = 'race a car';
  expect(isPalindrome(s)).toBe(false);
});

function isPalindrome(s: string): boolean {
  s = s.replace(/[^a-z0-9]/gi, '').toLowerCase();

  let i = 0;
  let j = s.length - 1;

  while (i <= j) {
    if (s[i] !== s[j]) {
      return false;
    }

    i++, j--;
  }

  return true;
}
