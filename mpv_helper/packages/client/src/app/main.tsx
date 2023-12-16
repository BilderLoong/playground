"use client";
import React, { RefObject, useEffect, useRef } from "react";
import { Command, keyMessage } from "server/protocols/ws";

enum NO_RETRY {
  UNMOUNT = 3000,
}

export const Main = (props: {}) => {
  const wsRef = useRef<WebSocket>();
  useEffect(() => {
    // TODO make the link dynamic.
    const WS_ADDRESS = "ws://localhost:8080";

    const ws = new WebSocket(WS_ADDRESS);
    wsRef.current = ws;

    document.addEventListener("keydown", handleKeydown);

    const onclose: WebSocket["onclose"] = async ({ code }) => {
      console.log("WS closed.");
      if (code === NO_RETRY.UNMOUNT) {
        console.log("Don't retry.");
        return;
      }

      await new Promise<void>((resolve, reject) => {
        const ws = new WebSocket(WS_ADDRESS);
        ws.onopen = function () {
          console.log("Reconnected!");
          ws.onopen = null;
          ws.onclose = onclose;
          wsRef.current = ws;
        };
        ws.onclose = function () {
          reject();
        };
        ws.onclose = function () {
          reject();
        };
      });
    };

    wsRef.current.onclose = onclose;

    return () => {
      wsRef.current?.close(NO_RETRY.UNMOUNT);
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
