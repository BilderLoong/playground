test('2 + 2 is 4', () => {
  expect(2 + 2).toBe(4); //expect return a expectation object
  //          ^ matcher // toBe() using Object.is() to compare
});

test('Difference between toBe and toEqual', () => {
  const obj = {
    data: 1,
  };
  expect(obj).not.toBe({ data: 1 }); // using Object.is()

  expect(obj).toEqual({ data: 1 });
});

test('should null', () => {
  const n = null;
  expect(n).toBeNull();
  expect(n).toBeDefined(); // match the value except undefined
  expect(n).not.toBeUndefined();
  expect(n).not.toBeTruthy(); // Truthy value
  expect(n).toBeFalsy(); // Truthy value
});

test('should zero', () => {
  const z = 0;
  expect(z).not.toBeNull();
});
