"use client";
import React, { RefObject, useEffect, useRef } from "react";
import { Command, keyMessage } from "server/protocols/ws";

export const Main = (props: {}) => {
  const wsRef = useRef<WebSocket>();
  useEffect(() => {
    // TODO make the link dynamic.
    const WS_ADDRESS = "ws://localhost:8080";

    const ws = new WebSocket(WS_ADDRESS);
    wsRef.current = ws;

    document.addEventListener("keydown", handleKeydown);

    return () => {
      ws.close();
      document.removeEventListener("keydown", handleKeydown);
    };
  }, []);

  const handleKeydown = (event: KeyboardEvent) => {
    wsRef.current?.send(keyMessageFactory(event.key));
  };

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
