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
import { P } from "ts-pattern";

describe.concurrent("startUnixDomainSocketServer", () => {
  let testSocketPath: string;
  beforeEach(() => {
    if (fs.existsSync(testSocketPath)) {
      fs.unlinkSync(testSocketPath);
    }
  });

  it("should create a socket.", async () => {
    testSocketPath = `/tmp/test-${Math.random()}.sock`;
    const { server, socketPath } = await startUnixDomainSocketServer(
      testSocketPath
    );

    expect(server).toBeDefined();
    expect(fs.existsSync(socketPath)).toBeTruthy();

    server.close();
  });

  test("client should communicate well with the server.", async () => {
    testSocketPath = `/tmp/test-${Math.random()}.sock`;
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

describe.concurrent("pipeBetweenSocketAndWS", () => {
  interface LocalTestContext {
    wss: WebSocket.Server;
    socketServer: MyServer;
    wsClient: WebSocket;
    socketClient: net.Socket;
    testWSPort: number;
  }

  beforeEach<LocalTestContext>(async (context) => {
    console.info("beforeEach running.");

    const testWSPort = Math.round(Math.random() * 10000);
    const testSocketPath = `/tmp/test-${testWSPort}.sock`;

    if (fs.existsSync(testSocketPath)) {
      fs.unlinkSync(testSocketPath);
    }

    context.testWSPort = testWSPort;

    context.wss = await startWSS({ port: testWSPort });
    context.socketServer = (
      await startUnixDomainSocketServer(testSocketPath)
    ).server;

    context.wsClient = new WebSocket(`ws://localhost:${testWSPort}`);
    context.socketClient = net.createConnection({ path: testSocketPath });

    console.info("beforeEach end.");
  });

  afterEach<LocalTestContext>(
    async ({ wss, socketServer, wsClient, socketClient }) => {
      console.info("afterEach running.");

      await Promise.all([
        util.promisify(wss.close),
        util.promisify(socketServer.close),
        util.promisify(wsClient.close),
        util.promisify(socketClient.end),
      ]);

      console.info("afterEach end.");
    }
  );

  it<LocalTestContext>("should pipe messages between WebSocket and TCP socket", async ({
    socketServer,
    wss,
    wsClient,
    socketClient,
  }) => {
    const dataFromWSClient2Socket = "Hello from websocket client!";
    const dataFromSocket2WSClient = "Hello from socket!";
    const options = {
      onWSSReceiveMsg: vi.fn((data) => data.toString()),
      onSocketServerReceiveMsg: vi.fn((data) => data.toString()),
    };
    // Start the piping function
    pipeBetweenSocketAndWS(wss, socketServer, options);

    await Promise.all([
      new Promise<void>((done) => socketClient.on("connect", done)),
      new Promise<void>((done) => wsClient.on("open", done)),
    ]);

    const promises = Promise.all([
      new Promise((resolve) => {
        socketClient.on("data", (data) => {
          resolve(data.toString());
        });
      }),

      new Promise((resolve) => {
        wsClient.on("message", (message) => {
          resolve(message.toString());
        });
      }),
    ]);

    // Send data to Websocket server.
    wsClient.send(dataFromWSClient2Socket);
    // Send data to unix domain socket server.
    socketClient.write(dataFromSocket2WSClient);
    const [socketClientReceivedData, wsClientReceivedData] = await promises;

    expect(socketClientReceivedData).toBe(dataFromWSClient2Socket);
    expect(options.onSocketServerReceiveMsg).toHaveBeenCalledOnce();
    expect(wsClientReceivedData).toBe(dataFromSocket2WSClient);
    expect(options.onSocketServerReceiveMsg).toHaveBeenCalledOnce();
    expect.assertions(4);
  });

  /* 
    Why? https://stackoverflow.com/questions/78015217/why-does-switching-the-position-of-async-code-cause-infinite-execution-in-node-j
  */
  test.skip<LocalTestContext>("Why this test stuck", async ({
    wsClient,
    socketClient,
  }) => {
    await new Promise<void>((done) => wsClient.on("open", done));
    await new Promise<void>((done) => socketClient.on("connect", done));
  });

  test<LocalTestContext>("Why this test does't stuck", async ({
    socketClient,
    wsClient,
  }) => {
    await new Promise<void>((done) => socketClient.on("connect", done));
    await new Promise<void>((done) => wsClient.on("open", done));
  });

  test<LocalTestContext>("Why this test does't stuck", async ({
    socketClient,
    wsClient,
  }) => {
    await Promise.all([
      new Promise<void>((done) => socketClient.on("connect", done)),
      new Promise<void>((done) => wsClient.on("open", done)),
    ]);
  });
});
