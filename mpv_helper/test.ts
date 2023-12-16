import cp, { ChildProcess, SpawnOptions } from "child_process";
import { spawn } from "child_process";

// cp.exec('ls')
// cp.spawnSync("date");
const p = cp
  .spawn("mpv", { stdio: "pipe" })
  .on("spawn", () => {
    console.log("spawn");
  })
  .on("exit", () => {
    console.log("exit");
  })
  .on("close", () => {
    console.log("close");
  });
p.stdout.setEncoding("utf8").on("data", (x) => {
  console.log(x);
});

const child = spawn("ls");

child.on("spawn", () => {
  console.log("Child process spawned");
});
