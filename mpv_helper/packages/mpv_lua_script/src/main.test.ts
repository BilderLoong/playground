import { pipeBetweenSocketAndWS, startUnixDomainSocketServer } from "./main";
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
import { log } from "node:console";
import util from "util";
import exp from "node:constants";

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

  test("client communicates well with server.", async () => {
    const { server, socketPath: socketPath } =
      await startUnixDomainSocketServer(testSocketPath);

    const handleClientData = vi.fn();
    server.on("connection", (socket) => {
      socket.setEncoding("utf8");
      socket.on("data", (data) => {
        handleClientData(data);
      });
      socket.write("Hello from server.");
    });

    const handleServerData = vi.fn();
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

    client.destroy();
    server.close();
    expect(handleClientData).toHaveBeenCalledWith("Hello from client.");
    expect(handleServerData).toHaveBeenCalledWith("Hello from server.");
  });
});

describe("pipeBetweenSocketAndWS", () => {
  let wss: WebSocket.Server;
  let socketServer: net.Server;
  let wsClient: WebSocket;
  let socketClient: net.Socket;
  const testPort = 12345;
  const testSocketPath = "/tmp/test.sock";

  beforeAll(async () => {
    if (fs.existsSync(testSocketPath)) {
      fs.unlinkSync(testSocketPath);
    }

    // Set up WebSocket server
    wss = new WebSocketServer({ port: testPort });

    // Set up TCP socket server
    socketServer = net.createServer().listen(testSocketPath);
    await new Promise((done) => {
      socketServer.on("listening", done);
    });
  });

  afterAll(async () => {
    await Promise.all([
      util.promisify(wsClient.close),
      util.promisify(socketClient.end),
      util.promisify(wss.close),
      util.promisify(socketServer.close),
    ]);
  });

  it("should pipe messages between WebSocket and TCP socket", async () => {
    const dataFromWS2Socket = "Hello from websocket!";
    const dataFromSocket2WS = "Hello, from socket!";

    const options = {
      onReceiveWSSMsg: vi.fn((data) => {
        return data.toString();
      }),
      onReceiveSocketMsg: vi.fn((data) => {
        return data.toString();
      }),
    };

    // Start the piping function
    pipeBetweenSocketAndWS(wss, socketServer, options);

    // Send data to Websocket server.
    wsClient = new WebSocket(`ws://localhost:${testPort}`);
    await new Promise<void>((done) => wsClient.on("open", done));
    wsClient.send(dataFromWS2Socket);

    // Send data to unix domain socket server.
    socketClient = net.createConnection({ path: testSocketPath });
    await new Promise<void>((done) => socketClient.on("connect", done));
    socketClient.write(dataFromSocket2WS);

    socketClient.on("data", (data) => {
      // TODO - The below expect doesn't run.
      expect(data.toString()).toBe(dataFromWS2Socket);
      expect(options.onReceiveSocketMsg).toHaveBeenCalledOnce();
    });

    wss.on("connection", (ws) => {
      ws.on("message", (message) => {
        // TODO - The below expect doesn't run.
        expect(message).toBe(dataFromSocket2WS);
        expect(options.onReceiveSocketMsg).toHaveBeenCalledOnce();
      });
    });
  });
});
