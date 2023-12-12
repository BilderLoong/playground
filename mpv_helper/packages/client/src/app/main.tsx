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
    const handleKeydown = (event: KeyboardEvent) => {
      ws.send(keyMessageFactory(event.key));
    };

    document.addEventListener("keydown", handleKeydown);

    return () => {
      ws.close();
      document.removeEventListener("keydown", handleKeydown);
    };
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

function keepConnect(
  address: string,
  ref: React.MutableRefObject<WebSocket | undefined>,
): React.MutableRefObject<WebSocket | undefined> {
  const ws = new WebSocket(address);
  ref.current = ws;
  // ws.onclose = function (ev) {
  //   const { code } = ev;
  //   console.log("WebSocket connection closed with: ", ev);
  //   setTimeout(() => keepConnect(address, ref), 100);
  // };
  return ref;
}
