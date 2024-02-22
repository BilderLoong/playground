local uv    = require 'luv'
local utils = require 'src.utils'


local script_path = utils.script_path()

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
    os.exit(1)
end)







uv.run("default")

local function pre_exit_clean_up()
    local function close_spawned_process(signal)
        print("got " .. signal .. ", shutting down")
        if handle then
            -- Kill the spawned process
            uv.process_kill(handle, 'SIGTERM')
        end
    end

    local signal = uv.new_signal()
    -- Register a signal handler for the process exit
    uv.signal_start(signal, "sigint", close_spawned_process)
    uv.signal_start(signal, "sigterm", close_spawned_process)
end
