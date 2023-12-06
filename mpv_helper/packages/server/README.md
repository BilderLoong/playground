## Testing scripts

```shell
mpv http://commondatastorage.googleapis.com/gtv-videos-bucket/sample/BigBuckBunny.mp4 --sub-file=https://gotranscript.com/samples/captions-example.srt --no-video --input-ipc-server=/tmp/mpv  

echo '{ "command": ["get_property", "playback-time"] }' | socat - /tmp/mpv


echo '{ "command": ["script-message", "mpvacious-copy-sub-to-clipboard"] }' | socat - /tmp/mpv


echo '{ "command": ["script-message", "mpvacious-copy-sub-to-clipboard"] }' | socat - /tmp/mpv

```
