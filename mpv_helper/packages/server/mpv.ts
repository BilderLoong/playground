import Mpv from "mpv";

(async function () {
  const mpv = await Mpv({
    path: "mpv",
    args: ["--audio-display=no", "--no-video", "--no-audio"],
  });

  mpv.command(
    "loadfile",
    "http://commondatastorage.googleapis.com/gtv-videos-bucket/sample/BigBuckBunny.mp4",
  );

  mpv.command("srcipt-mesasge");
})();
const socket = await startMpv({
  videoPath:
    "fixtures/sample.mp3",
});
