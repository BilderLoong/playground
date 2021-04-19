let counter = 1;
beforeEach(() => {
  console.log(`start of test ${counter}`);
});

afterEach(() => {
  console.log(`end of test ${counter}`);
  counter++;
});

beforeEach

test('1 test', () => {
  expect(counter).toBe(1);
});

test('2 test', () => {
  expect(counter).toBe(2);
});
test('3 test', () => {
  expect(counter).toBe(3);
});
test('4 test', () => {
  expect(counter).toBe(4);
});