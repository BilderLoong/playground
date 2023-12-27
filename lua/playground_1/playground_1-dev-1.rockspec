package = "playground_1"
version = "dev-1"
source = {
   url = "git+https://github.com/BilderLoong/playground.git"
}
description = {
   homepage = "https://github.com/BilderLoong/playground",
   license = "*** please specify a license ***"
}
dependencies = {
   "lua >= 5.1, < 5.5",
      "inspect >= 3.1"
}
build = {
   type = "builtin",
   modules = {
      main = "main.lua",
      print_stdin = "print_stdin.lua"
   }
}
