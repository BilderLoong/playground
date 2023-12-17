"use client";
import React, { RefObject, useEffect, useRef } from "react";
import { Command, keyMessage } from "server/protocols/ws";

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
      wsRef.current?.close(WS_CLOSE_CODE.UNMOUNT);
      document.removeEventListener("keydown", handleKeydown);
    };

    function handleKeydown(event: KeyboardEvent) {
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

    retry();
  });

  return ws;

  function retry() {
    const newWS = new WebSocket(address);
    console.log("connecting...");

    newWS.addEventListener("close", () => {
      console.log("closed.");
      setTimeout(retry);
    });

    newWS.addEventListener("open", () => {
      console.log("opened.");
      onRetrySuccessfully(newWS);
    });
  }
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
