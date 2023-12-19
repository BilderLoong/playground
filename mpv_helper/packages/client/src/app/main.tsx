"use client";
import { retry } from "@/utils";
import React, { RefObject, useEffect, useRef } from "react";
import { Command, keyMessage } from "server/protocols/ws";
import { startMpv } from "./actions";

enum WS_CLOSE_CODE {
  UNMOUNT = 3000,
}

export const Main = (props: {}) => {
  const wsRef = useRef<WebSocket>();
  useEffect(() => {
    // TODO make the link dynamic.
    const WS_ADDRESS = "ws://localhost:8080";

    document.addEventListener("keydown", handleKeydown);

    wsRef.current = createRetryableWS(WS_ADDRESS, (ws) => {
      wsRef.current = ws;
    });

    return () => {
      wsRef.current!.close(WS_CLOSE_CODE.UNMOUNT);
      document.removeEventListener("keydown", handleKeydown);
    };

    async function handleKeydown(event: KeyboardEvent) {
      const { key } = event;

      if (key === "s") {
        const mpv = await startMpv({
          videoFileURL:
            "http://commondatastorage.googleapis.com/gtv-videos-bucket/sample/BigBuckBunny.mp4",
          WSPort: 8080,
          subtitleURL: "https://gotranscript.com/samples/captions-example.srt",
        });

        console.log({ mpv });
      }
      if (wsRef.current && wsRef.current.readyState === wsRef.current.OPEN) {
        wsRef.current.send(keyMessageFactory(event.key));
      }
    }
  }, []);

  return <div>Click some key.</div>;
};

export function createRetryableWS(
  address: string,
  onRetrySuccessfully: (ws: WebSocket) => void,
): WebSocket {
  const ws = new WebSocket(address);

  ws.addEventListener("close", (ev: CloseEvent) => {
    if (ev.code === WS_CLOSE_CODE.UNMOUNT) {
      return;
    }

    retry(
      () =>
        new Promise<void>((resolve, rejects) => {
          const newWS = new WebSocket(address);
          console.log("connecting...");

          newWS.addEventListener("close", () => {
            console.log("closed.");
            rejects();
          });

          newWS.addEventListener("open", () => {
            resolve();
            console.log("opened.");
            onRetrySuccessfully(newWS);
          });
        }),
      { retries: 100, retryIntervalMs: 500 },
    );
  });

  return ws;
}

function keyMessageFactory(key: string) {
  return JSON.stringify(
    keyMessage.parse({
      command: Command.key,
      data: {
        key,
      },
    }),
  );
}
