/**
 * @param {number} x
 * @return {boolean}
 */
var isPalindrome = function (x) {
  const str = x.toString();

  for (let left = 0, right = str.length - 1; left < right; left++, right--) {
    if (str[left] !== str[right]) return false;
  }

  return true;
};

var isPalindrome = function (x) {
  if ((x % 10 === 0 && x !== 0) || x < 0) return false;

  let revertedNumber = 0;
  while (revertedNumber < x) {
    revertedNumber = revertedNumber * 10 + (x % 10);
    x = Math.floor(x / 10);
  }

  return revertedNumber === x || Math.floor(revertedNumber / 10) === x;
};
