export const c = 1 + 1;
export function foo() {
  if (c) {
    console.log(123);
    return;
  }
  return c;
}
