import { Hook, SyncHook } from "tapable";

class MyTappable {
  hooks = {
    hookWithArgs: new SyncHook(["newSpeed"]),
    normalHook: new SyncHook(),
  };

  runNormalHook() {
    this.hooks.normalHook.call("123");
  }
  runHookWithArgs() {
    this.hooks.hookWithArgs.call("123");
  }

  runAllHooks() {
    this.runHookWithArgs();
    this.runNormalHook();
  }
}

const myTappable = new MyTappable();

myTappable.hooks.normalHook.tap("MyPlugin", (args) => {
  console.log("MyPlugin brake");
  console.table(args);
});

myTappable.hooks.hookWithArgs.tap("MyPlugin", (args) => {
  console.log("MyPlugin brake");
  console.table(args);
});

myTappable.runAllHooks();
