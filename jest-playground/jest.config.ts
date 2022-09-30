module.exports = {
  preset: 'ts-jest',
  testEnvironment: 'node',
  /**
   * https://jestjs.io/ja/docs/configuration#testregex-string--arraystring
   */
  // testRegex: [
  //   '(/__tests__/.*|(\\.|/)(test|spec))\\.[jt]sx?$,', // Default value
  //   '.*/Leetcode/.*\\.[jt]s$', // For LeetCode problems test
  // ],
  testPathIgnorePatterns: [],
};
