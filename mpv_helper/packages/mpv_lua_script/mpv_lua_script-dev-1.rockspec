---@diagnostic disable: lowercase-global

package = "mpv_lua_script"
version = "dev-1"
source = {
   url = "git+https://github.com/BilderLoong/playground.git"
}
description = {
   homepage = "https://github.com/BilderLoong/playground",
   license = "*** please specify a license ***"
}
build = {
   type = "builtin",
   modules = {
      main = "main.lua",
      ["scripts.start_mpv"] = "scripts/start_mpv.lua"
   }
}

dependencies = {
   "json.lua",
   "inspect",
   "luv",
   "luasocket"
}
