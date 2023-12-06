import Mpv from "mpv";

(async function () {
  const mpv = await Mpv({
    path: "mpv",
    args: [
      "--audio-display=no",
      // "--no-video",
      // "--no-audio",
      "--window-minimized=yes",
    ],
  });

  mpv.command(
    "loadfile",
    "http://commondatastorage.googleapis.com/gtv-videos-bucket/sample/BigBuckBunny.mp4",
  );

  // Bun.serve({
  //   port: 8000,
  //   fetch(req, server) {
  //     // upgrade the request to a WebSocket
  //     if (server.upgrade(req)) {
  //       return; // do not return a Response
  //     }
  //     return new Response("Bun!");
  //   },
  //
  //   websocket: {
  //     message(ws, message) {},
  //     open(ws) {},
  //     drain(ws) {},
  //   },
  // });
})();
