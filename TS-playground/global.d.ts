type AnyFunction = (...args: unknown[]) => unknown;
interface Function {
  myBind: <T>(context: any, ...args: T[]) => AnyFunction;
  myCall: <T>(context: any, ...args: T[]) => any;
}
