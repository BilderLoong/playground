"use client";
import React, { useEffect, useRef } from "react";

export const Main = (props: {}) => {
  const wsRef = useRef<WebSocket>();
  useEffect(() => {
    // TODO make the link dynamic.
    const ws = new WebSocket("ws://localhost:8080");
    wsRef.current = ws;
    const handleKeydown = (event: KeyboardEvent) => {
      ws.send(event.key);
    };
    document.addEventListener("keydown", handleKeydown);

    return () => {
      ws.close();
      document.removeEventListener("keydown", handleKeydown);
    };
  }, []);

  return <div>Click some key.</div>;
};
