local init = {}

--- Get directory path that the script located.
---@return string
local function get_script_dir_path()
    local str = debug.getinfo(2, "S").source:sub(2)
    local path = str:match("(.*/)") or "./"

    return path .. "?.lua"
end

--- Add the `lua_modules` to the `package.path` and `package.cpath`.
---@return { path: string, cpath: string }
local function get_lua_modules_path_table()
    local version = _VERSION:match("%d+%.%d+")

    return {
        path = 'lua_modules/share/lua/' .. version .. '/?.lua;lua_modules/share/lua/' .. version .. '/?/init.lua',
        cpath = 'lua_modules/lib/lua/' .. version .. '/?.so'
    }
end


function init.run()
    package.path = package.path .. ";" .. get_script_dir_path() .. ";" .. get_lua_modules_path_table().path
    package.cpath = package.cpath .. ';' .. get_lua_modules_path_table().cpath
end

init.run()


--- Above code add necessary path.
print("package.path: " .. package.path)

local json    = require 'json'
local inspect = require 'inspect'
local utils   = require('utils')
local uv      = require 'luv'

--- @param started? fun(client): nil
local function start_web_ui(started)
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
    --- @param on_connected? fun(client:any): nil
    local function connect_to_socket(socket_path, on_connected)
        local client = uv.new_pipe(true)

        uv.pipe_connect(client, socket_path, function(err)
            if err == 'ENOENT' then
                print("Retring to connect to socket " .. socket_path .. " with error: ", err)
                utils.setTimeout(200, function()
                    connect_to_socket(socket_path, on_connected)
                end)

                return
            end
            if on_connected then
                on_connected(client)
            end
        end)
    end



    local socket_path = get_socket_path "mpv_helper.socket"
    local node_handle = start_node(socket_path, 5140)

    connect_to_socket(socket_path, function(client)
        uv.read_start(client, function(err, msg)
            assert(not err, err)
            if msg and started then
                started(client)
            else
                print("client end")
            end
        end)
    end)

    -- uv.run("default")
end

local state = {
    is_running = false
}

local success, mp = pcall(require, "mp")
-- Executed outside of the mpv.
if not success then
    start_web_ui();
    return
end

--- @param msg string
local function handle_socket_msg(msg)
    mp.commandv('keypress', msg)
end


local function _start_web_ui()
    if (state.is_running) then
        return
    end

    state.is_running = true

    start_web_ui(
        function(client)
            client:read_start(function(err, msg)
                handle_socket_msg(msg)
            end)
        end
    )

    mp.add_periodic_timer(0.001, function()
        -- Use nowait so that it won't block the loop.
        uv.run('nowait')
    end)

    mp.register_event('shutdown', function()
        uv.stop()
    end)
end

if (mp.get_opt("mpvaciousWeb-debug") == "true") then
    mp.register_event('start-file', function()
        _start_web_ui()
    end)
end


mp.add_key_binding("-", "mpvacious-web-interface", _start_web_ui)
