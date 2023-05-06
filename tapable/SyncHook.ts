import { Hook, SyncHook } from "tapable";

class MyTappable {
  hooks = {
    hookWithArgs: new SyncHook(["newSpeed"]),
    normalHook: new SyncHook(),
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

myTappable.hooks.normalHook.tap("MyPlugin", () => {
  console.log("MyPlugin brake");
});

myTappable.hooks.hookWithArgs.tap("MyPluginWithArgs", (args) => {
  console.log(`MyPluginWithArgs:${args}`);
});

// myTappable.runAllHooks();
myTappable.hooks.hookWithArgs.call("args!");
myTappable.hooks.normalHook.call("args!");
