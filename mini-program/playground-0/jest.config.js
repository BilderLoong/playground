module.exports = {
  preset: 'ts-jest',
  testEnvironment: 'jsdom',
  // For SecurityError: localStorage is not available for opaque origins with new testEnvironmentOptions.url option.
  // https://stackoverflow.com/questions/72392560/securityerror-localstorage-is-not-available-for-opaque-origins-with-new-testenv

  // testEnvironmentOptions: {
  //   url: 'http://localhost/',
  // },
  /**
   * https://jestjs.io/ja/docs/configuration#testregex-string--arraystring
   */
  // testRegex: [
  //   '(/__tests__/.*|(\\.|/)(test|spec))\\.[jt]sx?$,', // Default value
  //   '.*/Leetcode/.*\\.[jt]s$', // For LeetCode problems test
  // ],
  verbose: true,
};
