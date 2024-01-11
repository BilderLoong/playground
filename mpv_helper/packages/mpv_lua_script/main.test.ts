import { startUnixDomainSocketServer } from "./main";
import * as net from "node:net";

import fs from "fs";
import path from "path";

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
      await startUnixDomainSocketServer("testing/testSocket");

    expect(server).toBeDefined();

    expect(fs.existsSync(socketPath)).toBeTruthy();

    server.close();
  });

  test("client communicates well with server.", async () => {
    const { server, socketPath: socketPath } =
      await startUnixDomainSocketServer("testing/testSocket");

    const handleClientData = jest.fn();
    server.on("connection", (socket) => {
      socket.setEncoding("utf8");
      socket.on("data", (data) => {
        handleClientData(data);
      });
      socket.write("Hello from server.");
    });

    const handleServerData = jest.fn();
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
