Bun.serve({
  port: 8000,
  fetch(req, server) {
    // upgrade the request to a WebSocket
    if (server.upgrade(req)) {
      return; // do not return a Response
    }
    return new Response("Bun!");
  },

  websocket: {
    message(ws, message) {

    },
    open(ws) {
    },
    drain(ws) {},
  },
});
