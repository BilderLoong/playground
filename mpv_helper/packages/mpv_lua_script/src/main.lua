local uv      = require 'luv'
local json    = require 'json'
local inspect = require 'inspect'
local utils   = require('./utils')


--- @param  socket_name string
--- @param  ws_port number | string
local function start_node(socket_name, ws_port)
    local stdin       = uv.new_pipe(true)
    local stdout      = uv.new_pipe(true)
    local stderr      = uv.new_pipe(true)

    local handle, pid = uv.spawn('node', {
        args = {
            "-r", "esbuild-register",
            "--loader", "esbuild-register/loader",
            utils.script_path() .. 'main.ts',
            "--socket-name", socket_name,
            "--ws-port", ws_port
        },
        stdio = { stdin, stdout, stderr },
    }, function(code, signal) -- on exit
        print("exit code: ", code)
        print("exit signal: ", signal)
    end)


    uv.read_start(stdout, function(err, data)
        assert(not err, err)
        if data then
            print("stdout chunk: ", data)
        else
            print("stdout end: ")
        end
    end)

    uv.read_start(stderr, function(err, data)
        assert(not err, err)
        if data then
            print("stderr chunk: ", data)
        else
            print("stderr end: ")
        end
    end)

    if not handle then
        print("Error spawning child process: " .. pid)
        os.exit(1)
    end

    return handle
end

--- @param  socket_name string
local function connect_to_socket(socket_name)
    local client = uv.new_pipe(true)

    uv.pipe_connect(client, "/tmp/" .. socket_name, function(err)
        if err then
            print("Error connecting to socket: ", err)
            utils.setTimeout(1000, function()
                connect_to_socket(socket_name)
            end)
            return
        end


        print("Successfully connected to the Unix domain socket!")

        uv.read_start(client, function(err, data)
            assert(not err, err)
            if data then
                print("client chunk", data)
            else
                print("client end")
            end
        end)
    end)

    local message = 'Hello from mpv!'
    uv.write(client, message, function(err)
        -- Check for errors in writing
        if err then
            print("Error writing to socket:", err)
            return
        end

        print("Message sent to server:", message)
    end)
end



local socket_name = "mpv_helper.socket"
local node_handle = start_node(socket_name, 5140)

connect_to_socket(socket_name)

uv.run("default")
