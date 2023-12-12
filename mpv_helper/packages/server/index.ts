import Mpv from "mpv";
import { WebSocketServer } from "ws";
import { incommingMessage, Command } from "./protocols/ws";
import { match, P } from "ts-pattern";
import { resolve } from "node:path";

(async function () {
  const MPVAZIOUS_PATH = resolve("../mpvacious/");
  const mpv = await Mpv({
    path: "mpv",
    args: [
      // "--audio-display=no",
      // "--no-video",
      "--no-audio",
      "--window-minimized=yes",
      `--script=${MPVAZIOUS_PATH}`,
    ],
  });

  mpv.command(
    "loadfile",
    "http://commondatastorage.googleapis.com/gtv-videos-bucket/sample/BigBuckBunny.mp4",
  );

  mpv.on("file-loaded", () => {
    mpv.command(
      "sub-add",
      "https://gotranscript.com/samples/captions-example.srt",
      // resolve("./test.srt"),
    );
  });

  mpv.on("shutdown", () => {
    console.log("mpv shutdowned.");
  });

  const wss = new WebSocketServer({ port: 8080 });

  wss.on("connection", function connection(ws) {
    ws.on("error", console.error);

    ws.on("message", function message(data) {
      messageDispatcher(data.toString());
    });
  });

  function messageDispatcher(wsMsg: string) {
    const pareseMsg = incommingMessage.safeParse(JSON.parse(wsMsg));
    if (!pareseMsg.success) {
      return {
        message: `Unsupport message: ${wsMsg}`,
      };
    }

    match(pareseMsg.data).with(
      { command: Command.key, data: { key: P.select("key") } },
      ({ key }) => {
        match(key)
          .with("j", async (key) => {
            await mpv.command("script-message", "mpvacious-sub-seek-forward");
          })
          .with("k", async (key) => {
            await mpv.command("script-message", "mpvacious-sub-seek-back");
          })
          .with("a", async (key) => {
            await mpv.command("script-message", "mpvacious-menu-open");
          })
          .with("c", async (key) => {
            await mpv.command(
              "script-message",
              "mpvacious-copy-sub-to-clipboard",
            );
          });
      },
    );
  }
})();
