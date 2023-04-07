import shell = require("shelljs");

// shell.exec(
//   "echo 1 & false & echo 2",
//   { silent: true },
//   (code, stdout, stderr) => {
//     console.log("Exit code:", code);
//     console.log("Program output:", stdout);
//     console.log("Program stderr:", stderr);
//   }
// );

const child = shell.exec("false; >&2 echo error & echo next", {
  async: true,
  fatal: true,
  silent: true,
});

// stdout?.on("data", (data) => console.log(data));
// stderr?.on("data", (data) => {
//   console.error(data);
//   process.exit(1);
// });
// code?.on("exit", (code) => {
//   console.log("code is:", code);
// });
