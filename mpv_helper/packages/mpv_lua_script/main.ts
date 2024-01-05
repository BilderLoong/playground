import { WebSocketServer } from "ws";
import { match, P } from "ts-pattern";
import { Command, incomingMessage } from "../client/src/protocols/ws";
import * as net from "net";
import * as os from "os";

// main();

async function main() {
  const args = process.argv.slice(2);
  const [pipeName] = args;

  process.stdin.setEncoding("utf8");
  process.stdin.on("data", (data: string) => {
    // Process the input data
    const processedData = data.trim(); // Remove any leading/trailing whitespace

    // Write the processed data to stdout
    process.stdout.write(processedData + "\n");
  });

  process.stdin.on("end", () => {
    // The stdin stream has ended
    process.exit(0); // Exit the script
  });

  startWSS({ port: 8088 });
  const { server, socket } = await startNamedPipeServer(pipeName);
  socket.on("data", (data) => {
    console.log("123");
  });
}

function startWSS({ port }: { port: number }) {
  const wss = new WebSocketServer({ port });
  wss.on("connection", function connection(ws) {
    ws.on("error", console.error);

    ws.on("message", function message(data) {
      messageDispatcher(data.toString());
    });
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
