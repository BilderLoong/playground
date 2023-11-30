import { spawn } from "node:child_process";
import path from "node:path";

export async function startMpv({ videoPath }: { videoPath: string }): Promise<string> {
  const socketAddress = path.resolve("/", "tmp", "mpvsocket");
  const mpv = spawn("mpv", [videoPath, "--input-ipc-server=", socketAddress]);

  return new Promise((resolve, reject) => {
    mpv.stdout.on("data", (data) => {
      resolve(socketAddress);
      console.log(String(data));
    });

    mpv.stderr.on("data", (data) => {
      resolve(socketAddress);
      console.log(`stderr: ${data}`);
    });

    mpv.on("close", (code) => {
      reject();
      console.log(`child process exited with code ${code}`);
    });
  });
}

const socket =  startMpv({
  videoPath:
    "/Users/birudo/Resilio Sync/MaxLumi/Audio book/Basic_Economics,_Fifth_Edition_A_Common_Sense_Guide_to_the_Economy/001_Basic_Economics,Fifth_Edition_A_Common_Sense_Guide_to_the_Economy.mp3",
});
