local function script_path()
    local str = debug.getinfo(2, "S").source:sub(2)
    return str:match("(.*/)") or "./"
end

-- Get the script directory
local path = script_path()

-- Add the script directory to the package.path
package.path = package.path .. ";" .. path .. "?.lua"
