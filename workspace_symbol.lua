local clients = vim.lsp.get_active_clients()
local print = vim.print
local playground = require("custom.playground")

local tsserver_id = playground.start_tsserver()


local function handler(err, result, ctx, config)
	-- vim.print(err,result,ctx,config)
	print("result", result)
end

for _, client in ipairs(clients) do
	vim.print(client.name)

	client.request("workspace/symbol", { query = "didi" }, handler)
end
