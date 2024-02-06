import WebSocket, { WebSocketServer } from "ws";
import { match, P } from "ts-pattern";
import { Command, incomingMessage } from "../../client/src/protocols/ws";
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
import { MyServer } from "./myServer";

const isRunningAsAsMainModule =
  process.argv[1] === fileURLToPath(import.meta.url);
// https://stackoverflow.com/a/60309682
if (isRunningAsAsMainModule) {
  main();
}

async function main() {
  const { socketPath, wsPort } = getCommandLineOptions();
  process.stdin.setEncoding("utf8");
  process.stdin.on("end", () => {
    // The stdin stream has ended
    process.exit(0);
  });

  const wss = await startWSS({ port: wsPort });
  logger.info(`Websocket server start at port: ${wsPort}.
  You can test it by using:
    \`wscat -c ws://localhost:${wsPort}\``);
  const { server: socketServer } = await startUnixDomainSocketServer(
    socketPath
  );

  logger.info(`Listing at socket path: ${socketServer.address()}.
  You can test it by using:
     \`socat - UNIX-CONNECT:${socketServer.address()}\``);

  pipeBetweenSocketAndWS(wss, socketServer, {
    onSocketServerReceiveMsg(data) {
      logger.info(`${socketPath} received data: ${data.toString()}`);
      return data.toString();
    },
    onWSSReceiveMsg(data) {
      logger.info(`WSS received message: ${data.toString()}.`);
      return data.toString();
    },
  });

  prexit(() => {
    // https://nodejs.org/api/net.html#serverclosecallback
    socketServer.close();
  });
}

/**
 * Bridge between WebSocket and Socket
 * @param options.onReceiveWSSMsg Called when WebSocket received a message whose return value will be send to socket.
 * @param options.onReceiveSocketMsg Called when socket received a message whose return value will be send to WebSocket.
 */
export function pipeBetweenSocketAndWS(
  wss: WebSocketServer,
  socketServer: MyServer,
  options?: {
    onWSSReceiveMsg?: (
      data: WebSocket.RawData
    ) => Parameters<Socket["write"]>[0];
    onSocketServerReceiveMsg?: (
      data: Buffer
    ) => Parameters<WebSocket["send"]>[0];
  }
) {
  const toString = (data: { toString: () => string }) => data.toString();
  const { onWSSReceiveMsg = toString, onSocketServerReceiveMsg = toString } =
    options ?? {};

  wss.on("connection", async (ws) => {
    logger.info("A new websocket client connect to websocket server.");

    ws.on("message", (wsMsg) => {
      const res = onWSSReceiveMsg(wsMsg);

      logger.info(
        `Broadcasting a message from a ws client to to all connected socket clients. 
         All connected client size: ${socketServer.clients.size}.`
      );
      socketServer.clients.broadcast(res);
    });
  });

  socketServer.on("connection", (socket) => {
    logger.info("A new socket client connect to socket server.");

    socket.on("data", (data) => {
      const res = onSocketServerReceiveMsg(data);

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
    socketPath: z.string(),
    wsPort: z.coerce.number(),
  });

  program.option("--socket-path <str>").option("--ws-port <port>");
  program.parse();

  const optsParsedResult = Options.safeParse(program.opts());
  if (!optsParsedResult.success) {
    logger.error(`Invalid command line options: ${program.opts()}`);
    throw new Error(`Invalid command line options: ${program.opts()}`);
  }

  return optsParsedResult.data;
}

export function startWSS({ port }: { port: number }) {
  const wss = new WebSocketServer({ port });
  wss.on("connection", function connection(ws) {
    ws.on("error", logger.error);
  });

  return new Promise<WebSocketServer>((resolve, reject) => {
    wss.on("listening", () => {
      resolve(wss);
    });

    wss.on("error", reject);
  });
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
 * Create a unix domain socket server.
 */
export function startUnixDomainSocketServer(
  socketPath: string
): Promise<{ server: MyServer; socketPath: string }> {
  return new Promise((resolve, reject) => {
    const server = new MyServer();

    server.listen(socketPath, () => {
      resolve({ server, socketPath });
    });

    server.on("error", (err) => {
      reject(err);
    });
  });
}
