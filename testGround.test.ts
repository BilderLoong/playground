export const convertCapitalDashToCamel = (str: string) =>
  str
    .toLowerCase()
    .split('_')
    .reduce((p, c) => p + c[0].toUpperCase() + c.slice(1));

const helper = (arr: Array<string>) =>
  arr.reduce((p, c) => ({ ...p, [c]: convertCapitalDashToCamel(c) }), {});

describe('', () => {
  it('should', () => {
    const res = helper(['HU_ZI_LONG', 'HELLO_WORLD']);
    expect(res).toEqual({ HU_ZI_LONG: 'huZiLong', HELLO_WORLD: 'helloWorld' });
  });
});
