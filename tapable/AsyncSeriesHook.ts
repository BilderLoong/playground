import { AsyncSeriesHook, AsyncParallelHook } from "tapable";

class MyTappable {
  hooks = {
    asyncHookWithArgs: new AsyncSeriesHook(["arg1"]),
  };

  // runNormalHook() {
  //   this.hooks.normalHook.call("123");
  // }
  // runHookWithArgs() {
  //   this.hooks.hookWithArgs.call("123");
  // }

  // runAllHooks() {
  //   this.runHookWithArgs();
  //   this.runNormalHook();
  // }
}

const myTappable = new MyTappable();

myTappable.hooks.asyncHookWithArgs.tapAsync("async plugin 0", (arg1, cb) => {
  console.log("async plugin 0", arg1);
  console.timeLog("callAsync", 0);
  setTimeout(cb, 500);
});

myTappable.hooks.asyncHookWithArgs.tapAsync("async plugin 1", (arg1, cb) => {
  console.log("async plugin 1", arg1);
  console.timeLog("callAsync", "1");
  setTimeout(cb, 500);
});

console.time("callAsync");
myTappable.hooks.asyncHookWithArgs.callAsync("heihei", (arg) => {
  console.timeLog("callAsync", "heihei");
});
