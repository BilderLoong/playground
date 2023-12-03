## Setup 
Install depeendencies

```shell
luarocks install --deps-onl
```

## Troubleshooting
Encounter the below error:

```shell
luarocks install lipp/lua-websockets
Installing https://luarocks.org/manifests/lipp/lua-websockets-scm-1.rockspec
Cloning into 'lua-websockets'...
remote: Enumerating objects: 2198, done.
remote: Total 2198 (delta 0), reused 0 (delta 0), pack-reused 2198
Receiving objects: 100% (2198/2198), 1.08 MiB | 3.47 MiB/s, done.
Resolving deltas: 100% (1079/1079), done.

Error: Could not find header file for LIBWEBSOCKETS
  No file libwebsockets.h in /home/linuxbrew/.linuxbrew/include
  No file libwebsockets.h in /usr/include
  No file libwebsockets.h in /include
You may have to install LIBWEBSOCKETS in your system and/or pass LIBWEBSOCKETS_DIR or LIBWEBSOCKETS_INCDIR to the luarocks command.
Example: luarocks install lua-websockets LIBWEBSOCKETS_DIR=/home/linuxbrew/.linuxbrew

```
