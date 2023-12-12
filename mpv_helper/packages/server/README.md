## Testing scripts

```shell
mpv http://commondatastorage.googleapis.com/gtv-videos-bucket/sample/BigBuckBunny.mp4 --sub-file=https://gotranscript.com/samples/captions-example.srt --no-audio --window-minimized=yes --input-ipc-server=/tmp/mpv

echo '{ "command": ["get_property", "playback-time"] }' | socat - /tmp/mpv


echo '{ "command": ["script-message", "mpvacious-copy-sub-to-clipboard"] }' | socat - /tmp/mpv


echo '{ "command": ["script-message", "mpvacious-copy-sub-to-clipboard"] }' | socat - /tmp/mpv

echo '{ "command": ["sub-add", "https://gotranscript.com/samples/captions-example.srt"] }' | socat - /tmp/mpv

echo '{"request_id":3,"command":["srcipt-mesasge","mpvacious-sub-seek-forward"]}' | socat - /tmp/mpv
echo '{"command":["srcipt-mesasge","mpvacious-sub-seek-forward"]}' | socat - /tmp/mpv


```
