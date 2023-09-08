const EventEmitter = require("events");

class MyEmitter extends EventEmitter {
  constructor() {
    super();
    process.nextTick(() => {
      this.emit("event");
    });
    // setImmediate(() => {
    //   this.emit("event");
    // });
  }
}

const myEmitter = new MyEmitter();
myEmitter.on("event", () => {
  console.log("an event occurred!");
});

// console.log("start");

// setTimeout(() => {
//   console.log("setTimeout callback");
// }, 0);

// queueMicrotask(() => {
//   console.log("microtask callback");
// });

// process.nextTick(() => {
//   console.log("nextTick callback");
// });

// console.log("end");
