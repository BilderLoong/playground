import { nextTick } from "node:process";

Promise.resolve().then(() => console.log(1));
queueMicrotask(() => console.log(2));
nextTick(() => console.log(3));
