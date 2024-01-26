local uv      = require 'luv'
local json    = require 'json'
local inspect = require 'inspect'
local utils   = require('./utils')

local function get_socket_path(socket_name)
    local platform = os.getenv('OS')
    if platform == 'Windows_NT' then
        return '\\\\.\\pipe\\' .. socket_name
    else
        return '/tmp/' .. socket_name
    end
end


--- @param  socket_path string
--- @param  ws_port number | string
local function start_node(socket_path, ws_port)
    local stdin       = uv.new_pipe(true)
    local stdout      = uv.new_pipe(true)
    local stderr      = uv.new_pipe(true)

    local handle, pid = uv.spawn('node', {
        args = {
            "-r", "esbuild-register",
            "--loader", "esbuild-register/loader",
            utils.script_path() .. 'main.ts',
            "--socket-path", socket_path,
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

--- @param  socket_path string
local function connect_to_socket(socket_path)
    local client = uv.new_pipe(true)

    uv.pipe_connect(client, socket_path, function(err)
        if err == 'ENOENT' then
            print("Retring to connect to socket " .. socket_path .. " with error: ", err)
            utils.setTimeout(200, function()
                connect_to_socket(socket_path)
            end)

            return
        end


        print("Successfully connected to the Unix domain socket!")

        uv.read_start(client, function(err, data)
            assert(not err, err)
            if data then
                print("client chunk: ", data)
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



local socket_path = get_socket_path "mpv_helper.socket"
local node_handle = start_node(socket_path, 5140)

connect_to_socket(socket_path)

uv.run("default")
