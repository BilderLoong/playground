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

const mpv = Mpv({
  args: [], // Arguments to child_process.spawn,
  options: {}, // Options to child_process.spawn,
  path: "mpv", // Path of mpv (defaults to mpv or mpv.exe in path or cwd)
});
