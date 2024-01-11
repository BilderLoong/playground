import WebSocket, { WebSocketServer } from "ws";
import { match, P } from "ts-pattern";
import { Command, incomingMessage } from "../client/src/protocols/ws";
import z from "zod";
import * as net from "net";
import * as os from "os";
import * as fs from "fs";
import { program } from "commander";
import prexit from "prexit";
import { logger } from "./log";
import { Socket } from "net";
import { fileURLToPath } from "url";
import process from "process";

const isRunningAsAsMainModule =
  process.argv[1] === fileURLToPath(import.meta.url);
// https://stackoverflow.com/a/60309682
if (isRunningAsAsMainModule) {
  main();
}

async function main() {
  const { socketName, wsPort } = getCommandLineOptions();
  process.stdin.setEncoding("utf8");
  process.stdin.on("end", () => {
    // The stdin stream has ended
    process.exit(0);
  });

  const wss = startWSS({ port: wsPort });
  logger.info(`Websocket server start at ${wsPort}.
  You can test it by using:
    \`wscat -c ws://localhost:${wsPort}\``);
  const { server: socketServer } = await startUnixDomainSocketServer(
    getSocketPath(socketName)
  );
  const socketPath = socketServer.address();

  logger.info(`Listing at socket path: ${socketServer.address()}.
  You can test it by using:
     \`socat - UNIX-CONNECT:${socketServer.address()}\``);

  pipeBetweenSocketAndWS(wss, socketServer, {
    onReceiveSocketMsg(data) {
      logger.info(`${socketPath} received data: ${data.toString()}`);
      return data.toString();
    },
    onReceiveWSSMsg(data) {
      logger.info(`WS received message: ${data.toString()}.`);
      return data.toString();
    },
  });

  prexit(() => {
    // https://nodejs.org/api/net.html#serverclosecallback
    socketServer.close();
  });
}

export function pipeBetweenSocketAndWS(
  wss: WebSocketServer,
  socketServer: net.Server,
  options?: {
    onReceiveWSSMsg?: (
      data: WebSocket.RawData
    ) => Parameters<Socket["write"]>[0];
    onReceiveSocketMsg?: (data: Buffer) => Parameters<WebSocket["send"]>[0];
  }
) {
  const toString = (data: { toString: () => string }) => data.toString();
  const { onReceiveWSSMsg = toString, onReceiveSocketMsg = toString } =
    options ?? {};

  const socketPath = socketServer.address();
  if (typeof socketPath !== "string") {
    logger.error(`Wrong socket path: ${socketPath}.`);
    throw new Error("Socket server address must be a string.");
  }

  // const socketClientConnect =
  const socketClientPromise = new Promise<Socket>((resolve, reject) => {
    const socketClient = net.createConnection(socketPath, () => {
      resolve(socketClient);
    });

    socketClient.on("error", (err) => {
      logger.error(`Error connecting to socket server: ${err}`);
      reject(err);
    });
  });

  wss.on("connection", async (ws) => {
    logger.info("WebSocket client connected.");

    const socketClient = await socketClientPromise;

    ws.on("message", (wsMsg) => {
      socketClient.write(onReceiveWSSMsg(wsMsg));
      ws.on("close", socketClient.end);
    });
  });

  socketServer.on("connection", (socket) => {
    logger.info("Socket client connected.");

    socket.on("data", (data) => {
      const res = onReceiveSocketMsg(data);

      wss.clients.forEach((client) => {
        if (client.readyState !== WebSocket.OPEN) {
          return;
        }

        client.send(res);
      });
    });
  });
}

function getCommandLineOptions() {
  const Options = z.object({
    socketName: z.string(),
    wsPort: z.coerce.number(),
  });

  program.option("--socket-name <str>").option("--ws-port <port>");
  program.parse();

  const optsParsedResult = Options.safeParse(program.opts());
  if (!optsParsedResult.success) {
    logger.error(`Invalid command line options: ${program.opts()}`);
    throw new Error(`Invalid command line options: ${program.opts()}`);
  }

  return optsParsedResult.data;
}

function startWSS({ port }: { port: number }) {
  const wss = new WebSocketServer({ port });
  wss.on("connection", function connection(ws) {
    ws.on("error", logger.error);
  });

  return wss;
}

function messageDispatcher(wsMsg: string) {
  const parsedMsg = incomingMessage.safeParse(JSON.parse(wsMsg));
  if (!parsedMsg.success) {
    return {
      message: `Unsupported message: ${wsMsg}`,
    };
  }

  match(parsedMsg.data).with(
    { command: Command.key, data: { key: P.select("key") } },
    ({ key }) => {
      match(key)
        .with("j", async (key) => {
          process.stdout.write("j\n");
        })
        .with("k", async (key) => {})
        .with("a", async (key) => {})
        .with("c", async (key) => {});
    }
  );
}

/**
 * Create a new pipe
 */
export function startUnixDomainSocketServer(
  socketPath: string
): Promise<{ server: net.Server; socketPath: string }> {
  return new Promise((resolve, reject) => {
    const server = net.createServer();

    server.listen(socketPath, () => {
      resolve({ server, socketPath });
    });

    server.on("error", (err) => {
      reject(err);
    });
  });
}

function getSocketPath(socketName: string) {
  return os.platform() === "win32"
    ? `\\\\.\\pipe\\${socketName}`
    : `/tmp/${socketName}`;
}
