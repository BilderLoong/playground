# Mpv Lua Script

## Development `main.lua`

Before `main.lua`, run the below command to tell the lua using the installed lua_module

```shell
./lua_wrapper main.lua
```

## Development `main.ts`

```shell
pnpm run dev
```

### Testing

```shell
pnpm -w run test
```

### Troubleshooting

If you get below error when running `pnpm run dev`.

```shell
(node:60959) ExperimentalWarning: `--experimental-loader` may be removed in the future; instead use `register()`:
--import 'data:text/javascript,import { register } from "node:module"; import { pathToFileURL } from "node:url"; register("esbuild-register/loader", pathToFileURL("./"));'
(Use `node --trace-warnings ...` to show where the warning was created)
(node:60959) Warning: To load an ES module, set "type": "module" in the package.json or use the .mjs extension.
Waiting for the debugger to disconnect...
/Users/birudo/Project/playground/mpv_helper/packages/server/index.ts:1
import Mpv from "mpv";
^^^^^^

SyntaxError: Cannot use import statement outside a module
    at internalCompileFunction (node:internal/vm:77:18)
    at loadCJSModule (node:internal/modules/esm/translators:200:23)
    at ModuleWrap.<anonymous> (node:internal/modules/esm/translators:294:7)
    at ModuleJob.run (node:internal/modules/esm/module_job:218:25)
    at async ModuleLoader.import (node:internal/modules/esm/loader:329:24)
    at async loadESM (node:internal/process/esm_loader:34:7)
    at async handleMainPromise (node:internal/modules/run_main:113:12)

Node.js v20.10.0
```

It may cause by this [issue](https://github.com/egoist/esbuild-register/issues/96). Try lower node version instead.
