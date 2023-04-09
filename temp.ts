import { spawn } from "child_process";

const c1 = spawn("export a=1; echo $a", [], { stdio: "inherit", shell: true });
const c2 = spawn("echo $a", [], { stdio: "inherit", shell: true });

console.log({ c2 });
