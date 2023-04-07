import { Hook, SyncHook } from "tapable";

class MyTappable {
  hooks = {
    accelerate: new SyncHook(["newSpeed"]),
    brake: new SyncHook(),
    calculateRoutes: new SyncHook(),
  };

  startBreak() {
    this.hooks.accelerate.call("123");
  }
}

const myTappable = new MyTappable();

myTappable.hooks.brake.tap("MyPlugin", (args) => {
  console.log("MyPlugin brake");
  console.table(args);
});
