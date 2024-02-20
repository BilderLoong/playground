local uv          = require 'luv'
local utils       = require 'src.utils'

local stdin       = uv.new_pipe(true)
local stdout      = uv.new_pipe(true)
local stderr      = uv.new_pipe(true)

local script_path = utils.script_path()
print(script_path)

local handle, pid = uv.spawn('mpv', {
    args = {
        'http://commondatastorage.googleapis.com/gtv-videos-bucket/sample/BigBuckBunny.mp4',
        "--sub-file=https://gotranscript.com/samples/captions-example.srt",
        '--window-minimized=yes',
        '--input-ipc-server=/tmp/mpvsocket',
        '--script=' .. script_path .. '..' .. "/src",
    },
    -- stdio = { stdin, stdout, stderr },
    stdio = { 0, 1, 2 },
}, function(code, signal) -- on exit
    print("exit code", code)
    print("exit signal", signal)
end)

uv.run("default")
