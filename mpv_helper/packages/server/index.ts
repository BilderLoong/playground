import Mpv from "mpv";
import { WebSocketServer } from "ws";
import { incommingMessage, Command } from "./protocols/ws";
import { match, P } from "ts-pattern";

(async function () {
  const mpv = await Mpv({
    path: "mpv",
    args: [
      // "--audio-display=no",
      // "--no-video",
      "--no-audio",
      "--window-minimized=yes",
    ],
  });

  await mpv.command(
    "loadfile",
    "http://commondatastorage.googleapis.com/gtv-videos-bucket/sample/BigBuckBunny.mp4",
  );

  await mpv.command(
    "sub-add",
    "https://gotranscript.com/samples/captions-example.srt",
  );

  const wss = new WebSocketServer({ port: 8080 });

  wss.on("connection", function connection(ws) {
    console.log("new connection.");
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
