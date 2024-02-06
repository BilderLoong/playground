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

class ConnectedSockets extends Set<Socket> {
  /** Send message to all connected clients, except the given client.  */
  broadcast(data: string | Uint8Array, excepts?: Socket[]) {
    this.forEach((sock) => {
      if (excepts?.includes(sock)) {
        return;
      }
      if (!sock.writable) {
        logger.warn(
          `During socket server broadcasting, skipped this not writable socket: ${sock}.`
        );
        return;
      }

      sock.write(data);
    });
  }
}

export class MyServer extends net.Server {
  clients = new ConnectedSockets();
  constructor(connectionListener?: (socket: Socket) => void) {
    super(connectionListener);
    this.on("connection", (socket) => {
      this.clients.add(socket);

      socket.on("close", () => {
        logger.info("A socket client closed. Bye bye~~~");
        this.clients.delete(socket);
      });
    });
  }
}
