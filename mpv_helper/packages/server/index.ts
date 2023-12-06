import Mpv from "mpv";
import { WebSocketServer } from "ws";

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
    // "https://gotranscript.com/samples/captions-example.srt",
  );

  // await mpv.command(
  //   "sub-add",
  //   "https://gotranscript.com/samples/captions-example.srt",
  // );

  const wss = new WebSocketServer({ port: 8080 });

  wss.on("connection", function connection(ws) {
    console.log("new connection.");
    ws.on("error", console.error);

    ws.on("message", function message(data) {
      console.log("received: %s", data);
    });

    ws.send("something");
  });
})();
