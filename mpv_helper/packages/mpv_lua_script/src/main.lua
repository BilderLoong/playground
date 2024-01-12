local uv      = require 'luv'
local json    = require 'json'
local inspect = require 'inspect'
local utils   = require('utils')


local stdin  = uv.new_pipe(true)
local stdout = uv.new_pipe(true)
local stderr = uv.new_pipe(true)



local handle, pid = uv.spawn('node', {
    args = {
        "-r", "esbuild-register",
        "--loader", "esbuild-register/loader",
        utils.script_path() .. 'main.ts'
    },
    stdio = { stdin, stdout, stderr },
}, function(code, signal) -- on exit
    print("exit code", code)
    print("exit signal", signal)
end)


print("process opened", pid)

uv.read_start(stdout, function(err, data)
    assert(not err, err)
    if data then
        print("stdout chunk", data)
    else
        print("stdout end")
    end
end)

uv.read_start(stderr, function(err, data)
    assert(not err, err)
    if data then
        print("stderr chunk", data)
    else
        print("stderr end")
    end
end)


uv.run("default")
