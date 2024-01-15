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
  beforeEach(() => {
    createTempDir();
  });
  afterEach(() => {
    deleteTempDir();
  });

  it("should create a socket.", async () => {
    const { server, socketPath: socketPath } =
      await startUnixDomainSocketServer("/tmp/testSocket");

    expect(server).toBeDefined();

    expect(fs.existsSync(socketPath)).toBeTruthy();

    server.close();
  });

  test("client communicates well with server.", async () => {
    const { server, socketPath: socketPath } =
      await startUnixDomainSocketServer("/tmp/testSocket");

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
    // Set up WebSocket server
    wss = new WebSocketServer({ port: testPort });

    // Set up TCP socket server
    socketServer = net.createServer().listen(testSocketPath);
    await new Promise((done) => {
      socketServer.on("listening", done);
    });
  });

  afterAll(async () => {
    // Clean up servers and clients
    wsClient.close();
    socketClient.end();
    wss.close();
    socketServer.close();
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
    await new Promise<void>((done) => {
      wsClient.on("open", () => {
        wsClient.send(dataFromWS2Socket);
        done();
      });
    });

    // Send data to unix domain socket server.
    await new Promise<void>((done) => {
      socketClient = net.createConnection({ path: testSocketPath }, () => {
        socketClient.write(dataFromSocket2WS);
        done();
      });
    });

    socketClient.on("data", (data) => {
      expect(data.toString()).toBe(dataFromWS2Socket);
    });

    wss.on("connection", (ws) => {
      ws.on("message", (message) => {
        expect(message).toBe(dataFromSocket2WS);
      });
    });

    // expect(options.onReceiveSocketMsg).toHaveBeenCalledOnce();
    // expect(onReceiveWSSMsg).toHaveBeenCalledOnce();
    // expect(options.onReceiveWSSMsg).toHaveBeenCalledTimes(1);
  });
});
