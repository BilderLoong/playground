## Doc Caveat
Use second person pronoun in this doc.

## About Jest
After cost one afternoon time, this work space now able to use jest to test LeetCode problems.

### Usage
- I have add a debug config called: `Jest: current file` in launch.json, you can use this to run test and debug in current file.

- I also install the vscode-jest extension, so you should able to see the test result in the editor gutter.

### Note
#### Something should be write down as the reference for later
- The `jest <regexForTestFiles>` is not `jest <pathToFile>` (use `jest --runTestByPath=<path>` instead.).
- The Chinese file path may cause unexpected problems, this one cost lot of my time. 

#### Jest Note
##### Command Line Options
`--runTestByPath=<path>`: specify the file path which to run test.
##### Config Options
`testRegex`: The pattern or patterns Jest uses to detect test files. After my test. Files past from `--runTestByPath=<path>` or ` jest <regexForTestFiles>` still need to meet the restrict of this option no matter whether path is directories or files.

