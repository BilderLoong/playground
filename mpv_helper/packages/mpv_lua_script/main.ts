import { WebSocketServer } from "ws";
import { match, P } from "ts-pattern";
import { Command, incomingMessage } from "../client/src/protocols/ws";
import z from "zod";
import * as net from "net";
import * as os from "os";
import * as fs from "fs";
import { program } from "commander";
import prexit from "prexit";

main();

async function main() {
  const { pipName, wsPort } = getCommandLineOptions();
  process.stdin.setEncoding("utf8");
  process.stdin.on("end", () => {
    // The stdin stream has ended
    process.exit(0);
  });

  startWSS({ port: wsPort });

  const { server, pipePath } = await startNamedPipeServer(pipName);
  console.log(`Listing at pipe path: ${pipePath}.`);
  prexit(() => {
    fs.unlinkSync(pipePath);
  });

  server.on("connection", (socket) => {
    socket.on("data", (data) => {
      console.log("Node process received data.", data);
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
    console.error("Wrong command line options.");
    process.exit(1);
  }

  return optsParsedResult.data;
}

function startWSS({ port }: { port: number }) {
  const wss = new WebSocketServer({ port });
  wss.on("connection", function connection(ws) {
    ws.on("error", console.error);

    ws.on("message", function message(data) {
      messageDispatcher(data.toString());
    });
  });

  console.log(`Websocket server start at ${port}.`);

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
export function startNamedPipeServer(
  pipeName: string
): Promise<{ server: net.Server; pipePath: string }> {
  return new Promise((resolve, reject) => {
    const server = net.createServer((socket) => {});

    const pipePath =
      os.platform() === "win32"
        ? `\\\\.\\pipe\\${pipeName}`
        : `/tmp/${pipeName}`;

    server.listen(pipePath, () => {
      resolve({ server, pipePath });
    });

    server.on("error", (err) => {
      reject(err);
    });
  });
}
