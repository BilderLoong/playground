local uv = require('luv')

local utils = {}

local function is_win()
    return package.config:sub(1, 1) == '\\'
end

local function get_path_separator()
    if is_win() then
        return '\\'
    end
    return '/'
end

--- @return string
function utils.script_path()
    local str = debug.getinfo(2, 'S').source:sub(2)
    if is_win() then
        str = str:gsub('/', '\\')
    end
    return str:match('(.*' .. get_path_separator() .. ')') or ''
end

--- Creating a simple setTimeout wrapper
--- @param timeout number
--- @param callback function
---@return unknown
function utils.setTimeout(timeout, callback)
    local timer = uv.new_timer()
    timer:start(timeout, 0, function()
        timer:stop()
        timer:close()
        callback()
    end)
    return timer
end

return utils
