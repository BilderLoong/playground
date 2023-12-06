import { Socket, createServer } from "net";

const SOCKET_ADDRESS = `/tmp/${Math.random().toFixed(2)}`;

const server = createServer((c) => {
  // 'connection' listener.
  console.log("client connected");
  c.on("end", () => {
    console.log("client disconnected");
  });
  c.write("hello\r\n");
  c.pipe(c);
});

server.listen(SOCKET_ADDRESS);
server.on("listening", () => {
  console.log("listening at: ", SOCKET_ADDRESS);
});

const socket = new Socket();
socket.connect(SOCKET_ADDRESS);
