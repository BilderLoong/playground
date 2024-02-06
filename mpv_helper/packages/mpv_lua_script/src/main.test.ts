import {
  pipeBetweenSocketAndWS,
  startUnixDomainSocketServer,
  startWSS,
} from "./main";
import {
  expect,
  test,
  describe,
  beforeAll,
  afterAll,
  beforeEach,
  afterEach,
  it,
  vi,
} from "vitest";
import * as net from "node:net";
import WebSocket, { WebSocketServer } from "ws";
import { logger } from "./log";
import fs from "fs";
import path from "path";
import util from "util";
import { MyServer } from "./myServer";

function createTempDir() {
  const tmpDir = path.join("/tmp", "testing");
  if (!fs.existsSync(tmpDir)) {
    fs.mkdirSync(tmpDir);
  }
}

function deleteTempDir() {
  const tmpDir = path.join("/tmp", "testing");
  if (fs.existsSync(tmpDir)) {
    fs.rm(tmpDir, { recursive: true }, (err) => {
      if (!err) {
        return;
      }
      console.error(err);
    });
  }
}

describe("startUnixDomainSocketServer", () => {
  const testSocketPath = "/tmp/test.socket";

  beforeEach(() => {
    if (fs.existsSync(testSocketPath)) {
      fs.unlinkSync(testSocketPath);
    }
  });

  it("should create a socket.", async () => {
    const { server, socketPath: socketPath } =
      await startUnixDomainSocketServer(testSocketPath);

    expect(server).toBeDefined();

    expect(fs.existsSync(socketPath)).toBeTruthy();

    server.close();
  });

  test("client should communicate well with the server.", async () => {
    const { server, socketPath } = await startUnixDomainSocketServer(
      testSocketPath
    );

    const handleServerData = vi.fn();
    const handleClientData = vi.fn();

    server.on("connection", (socket) => {
      socket.setEncoding("utf8");
      socket.on("data", (data) => {
        handleClientData(data);
      });
      socket.write("Hello from server.");
    });

    const client = await new Promise<net.Socket>((resolve, reject) => {
      const client = net.connect({ path: socketPath }, () => {
        client.setDefaultEncoding("utf-8");
        client.write("Hello from client.");
      });

      client.on("data", (data) => {
        handleServerData(data.toString());
        resolve(client);
      });
    });

    await Promise.all([
      util.promisify(client.destroy),
      util.promisify(server.close),
    ]);

    expect(handleClientData).toHaveBeenCalledWith("Hello from client.");
    expect(handleServerData).toHaveBeenCalledWith("Hello from server.");
  });
});

describe("pipeBetweenSocketAndWS", () => {
  let wss: WebSocket.Server;
  let socketServer: MyServer;
  let wsClient: WebSocket;
  let socketClient: net.Socket;
  const testPort = 12345;
  const testSocketPath = "/tmp/test.sock";

  beforeAll(async () => {
    console.info("beforeAll running.");
    if (fs.existsSync(testSocketPath)) {
      fs.unlinkSync(testSocketPath);
    }

    ({ server: socketServer } = await startUnixDomainSocketServer(
      testSocketPath
    ));
    wss = await startWSS({ port: testPort });

    console.info("beforeAll end.");
  });

  afterAll(async () => {
    console.info("afterAll running.");
    await Promise.all([
      util.promisify(wsClient.close),
      util.promisify(socketClient.end),
      util.promisify(wss.close),
      util.promisify(socketServer.close),
    ]);
    console.info("afterAll end.");
  });

  it("should pipe messages between WebSocket and TCP socket", async () => {
    const dataFromWSClient2Socket = "Hello from websocket client!";
    const dataFromSocket2WSClient = "Hello from socket!";

    const options = {
      onWSSReceiveMsg: vi.fn((data) => {
        console.debug("WSS received message: ", data.toString());

        return data.toString();
      }),
      onSocketServerReceiveMsg: vi.fn((data) => {
        console.debug("Socket server received message: ", data.toString());
        return data.toString();
      }),
    };

    // Start the piping function
    pipeBetweenSocketAndWS(wss, socketServer, options);

    wsClient = new WebSocket(`ws://localhost:${testPort}`);
    await new Promise<void>((done) => wsClient.on("open", done));
    socketClient = net.createConnection({ path: testSocketPath });
    await new Promise<void>((done) => socketClient.on("connect", done));

    const socketClientDataPromise = new Promise((resolve) => {
      socketClient.on("data", (data) => {
        expect(data.toString()).toBe(dataFromWSClient2Socket);
        expect(options.onSocketServerReceiveMsg).toHaveBeenCalledOnce();
        resolve(data.toString());
      });
    });

    const wsClientMessagePromise = new Promise((resolve) => {
      wsClient.on("message", (message) => {
        expect(message.toString()).toBe(dataFromSocket2WSClient);
        expect(options.onSocketServerReceiveMsg).toHaveBeenCalledOnce();
        resolve(message.toString());
      });
    });

    // Send data to Websocket server.
    wsClient.send(dataFromWSClient2Socket);
    // Send data to unix domain socket server.
    socketClient.write(dataFromSocket2WSClient);

    await socketClientDataPromise;
    await wsClientMessagePromise;
    expect.assertions(4);
  });
});
