import { WebSocketServer } from "ws";
import { match, P } from "ts-pattern";
import { Command, incomingMessage } from "../client/src/protocols/ws";
import z from "zod";
import * as net from "net";
import * as os from "os";
import * as fs from "fs";
import { program } from "commander";
import prexit from "prexit";
import { logger } from "./log";

main();

async function main() {
  const { pipName: socketName, wsPort } = getCommandLineOptions();
  process.stdin.setEncoding("utf8");
  process.stdin.on("end", () => {
    // The stdin stream has ended
    process.exit(0);
  });

  const wss = startWSS({ port: wsPort });
  const socketPath = getSocketPath(socketName);
  const { server: socketServer } = await startUnixDomainSocketServer(
    socketPath
  );

  logger.info(`Listing at socket path: ${socketPath}.`);

  prexit(() => {
    // https://nodejs.org/api/net.html#serverclosecallback
    socketServer.close();
  });

  socketServer.on("connection", (socket) => {
    socket.on("data", (data) => {
      logger.info(`${socketPath} received data: `, data.toString());

      wss.on("connection", (ws) => {
        ws.on("message", (wsMsg) => {
          logger.info(`WS at ${wsPort} received message: `, wsMsg.toString());

          // socket.write(wsMsg.toString());
        });
      });
    });
  });
}

function getCommandLineOptions() {
  const Options = z.object({
    pipName: z.string(),
    wsPort: z.coerce.number(),
  });

  program.option("--pip-name <str>").option("--ws-port <port>");
  program.parse();

  const optsParsedResult = Options.safeParse(program.opts());
  if (!optsParsedResult.success) {
    logger.error("Wrong command line options.");
    process.exit(1);
  }

  return optsParsedResult.data;
}

function startWSS({ port }: { port: number }) {
  const wss = new WebSocketServer({ port });
  wss.on("connection", function connection(ws) {
    ws.on("error", logger.error);
  });

  logger.info(`Websocket server start at ${port}.`);

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
