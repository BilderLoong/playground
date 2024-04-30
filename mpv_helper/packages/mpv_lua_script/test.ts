import WebSocket, { WebSocketServer } from "ws";

const dataFromWS2Socket = "Hello from websocket!";
const dataFromSocket2WS = "Hello, from socket!";
const testPort = 12345;
const wsClient = new WebSocket(`ws://localhost:${testPort}`);
const wss = new WebSocketServer({ port: testPort });
// wsClient.send(dataFromWS2Socket);
wss.on("listening", () => {
  // Create the WebSocket client and connect it to the server.
  const wsClient = new WebSocket(`ws://localhost:${testPort}`);

  wsClient.on("open", () => {
    console.log("WebSocket opened");
    wsClient.send(dataFromWS2Socket);
  });

  wsClient.on("message", (dataFromSocket2WS) => {
    console.log(`Received message from server: ${dataFromSocket2WS}`);
  });
});



wss.on("connection", (ws) => {
  ws.on("message", (message) => {
    console.debug({ wssReceiveMessage: message.toString() });
  });
});
