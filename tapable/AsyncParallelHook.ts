import { AsyncSeriesHook, AsyncParallelHook } from "tapable";

class MyTappable {
  hooks = {
    asyncHookWithArgs: new AsyncParallelHook(["arg1"]),
  };
}

const myTappable = new MyTappable();

myTappable.hooks.asyncHookWithArgs.tapAsync("async plugin 0", (arg1, cb) => {
  console.timeLog("callAsync", 0);
  setTimeout(cb, 500);
});

myTappable.hooks.asyncHookWithArgs.tapAsync("async plugin 1", (arg1, cb) => {
  console.timeLog("callAsync", "1");
  setTimeout(cb, 500);
});

myTappable.hooks.asyncHookWithArgs.tapAsync("async plugin 2", (arg1, cb) => {
  console.timeLog("callAsync", "1");
  setTimeout(cb, 1000);
});

console.time("callAsync");
myTappable.hooks.asyncHookWithArgs.callAsync("heihei", (arg) => {
  console.timeLog("callAsync", "heihei");
});
