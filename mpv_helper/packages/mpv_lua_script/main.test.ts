import { startNamedPipeServer } from "./main";
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

describe("startNamedPipeServer", () => {
  beforeEach(() => {
    createTempDir();
  });
  afterEach(() => {
    deleteTempDir();
  });

  // const pipePath = "/tmp/testing/testPipe";

  it("should create a named pipe", async () => {
    const { server, pipePath } = await startNamedPipeServer("testing/testPipe");

    expect(server).toBeDefined();

    expect(fs.existsSync(pipePath)).toBeTruthy();

    server.close();
  });

  test("client communicates well with server.", async () => {
    const { server, pipePath } = await startNamedPipeServer("testing/testPipe");

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
      const client = net.connect({ path: pipePath }, () => {
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

  // it("should reject on server error", async () => {
  //   const mockError = new Error("test error");
  //   const mockServer = {
  //     listen: jest.fn(),
  //     on: jest.fn((event, callback) => callback(mockError)),
  //   };
  //   (net.createServer as jest.Mock).mockReturnValue(mockServer);

  //   const pipeName = "testPipe";

  //   await expect(startNamedPipeServer(pipeName)).rejects.toThrow(mockError);
  //   expect(net.createServer).toHaveBeenCalled();
  //   expect(mockServer.on).toHaveBeenCalledWith("error", expect.any(Function));
  // });
});
