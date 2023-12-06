"use client";
import React, { useEffect, useRef } from "react";

export const Main = (props: {}) => {
  const wsRef = useRef<WebSocket>();
  useEffect(() => {
    const ws = new WebSocket("ws://localhost:8080");
    wsRef.current = ws;
    return () => {
      ws.close();
    };
  }, []);

  return (
    <button
      onClick={() => {
        wsRef.current?.send("dodo");
      }}
    >
      send message.
    </button>
  );
};
