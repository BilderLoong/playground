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

import fs from "fs";
import path from "path";
import { aw } from "vitest/dist/reporters-qc5Smpt5.js";

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
    await new Promise<void>((done) => {
      socketServer = net.createServer().listen(testSocketPath, done);
    });
  });

  afterAll(async () => {
    // Clean up servers and clients
    wsClient.close();
    socketClient.end();
    wss.close();
    fs.unlinkSync(testSocketPath);
    new Promise((done) => {
      socketServer.close(done);
    });
  });

  it("should pipe messages between WebSocket and TCP socket", async () => {
    const testData = "Hello, world!";

    // Start the piping function
    pipeBetweenSocketAndWS(wss, socketServer, {
      onReceiveWSSMsg: (data) => data.toString().toUpperCase(),
      onReceiveSocketMsg: (data) => data.toString().toLowerCase(),
    });

    // Connect WebSocket client
    wsClient = new WebSocket(`ws://localhost:${testPort}`);
    const done = new Promise<void>((done) => {
      wsClient.on("open", async () => {
        // When the WebSocket client is connected, connect the TCP socket client
        socketClient = net.createConnection({ path: testSocketPath }, () => {
          // Send a message through the WebSocket client
          wsClient.send(testData);
        });

        socketClient.on("data", (data) => {
          // Expect the modified message from the TCP socket server
          expect(data.toString()).toBe(testData.toUpperCase());
          done();
        });
      });
    });

    // Listen to messages from the WebSocket server
    wss.on("connection", (ws) => {
      ws.on("message", (message) => {
        // Expect the modified message from the WebSocket client
        expect(message).toBe(testData.toLowerCase());
      });
    });

    // Send a message to the WebSocket server through the TCP socket
    socketServer.on("connection", (socket) => {
      socket.write(testData);
    });

    await done;
  });
});
