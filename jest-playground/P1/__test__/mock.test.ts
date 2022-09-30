// Reason for putting const declaration before import statement: https://jestjs.io/docs/manual-mocks#using-with-es-module-imports
const Default = {
  name: 'mockDefault',
};
const Add = 'mockedAdd';

import bar, { add } from '../test';

// Jest mock will be hoist.
jest.mock('../test', () => {
  return {
    // When using the factory parameter for an ES6 module with a **default** export, the __esModule: true property needs to be specified.
    __esModule: true, // https://jestjs.io/docs/jest-object#jestmockmodulename-factory-options
    default: Default,
    add() {
      return Add;
    },
  };
});

describe('mock function', () => {
  it('Learn jest.fn API', () => {
    const mock2 = jest.fn();
    console.log({ mock2: mock2 });
    console.log({ mock2GetMockImplementation: mock2.getMockImplementation() });
  });

  it('should mock function return value once correctly', () => {
    const mock1 = jest.fn();

    // Make the mock return `true` for the first call,
    // and `false` for the second call
    mock1.mockReturnValueOnce(true).mockReturnValueOnce(false);

    const result = [11, 12].filter((num) => mock1(num));
    expect(result).toEqual([11]);
    const calls = mock1.mock.calls;
    expect(calls[0][0]).toBe(11);
    expect(calls[1][0]).toBe(12);
  });
});

describe('mock module', () => {
  it('should mock the module', () => {
    expect(add(1, 1)).toBe(Add);
    expect(bar).toEqual(Default);
  });
});
