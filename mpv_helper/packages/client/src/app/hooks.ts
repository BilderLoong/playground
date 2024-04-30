import { useEffect, useRef } from "react";
import { retry } from "@/utils";
import { Command, keyMessage } from "@/protocols/ws";

enum WS_CLOSE_CODE {
  UNMOUNT = 3000,
}

export function useCommutateToWS() {
  const wsRef = useRef<WebSocket>();
  useEffect(() => {
    const WS_ADDRESS = "ws://localhost:8080";

    document.addEventListener("keydown", handleKeydown);
    wsRef.current = createRetryableWS(WS_ADDRESS, (ws) => {
      wsRef.current = ws;
    });

    return () => {
      wsRef.current!.close(WS_CLOSE_CODE.UNMOUNT);
      document.removeEventListener("keydown", handleKeydown);
    };
  }, []);

  function handleKeydown(event: KeyboardEvent) {
    if (!wsRef.current || wsRef.current?.readyState !== wsRef.current?.OPEN) {
      return;
    }

    wsRef.current.send(keyMessageFactory(buildMPVKeyName(event)));
  }
}

export function createRetryableWS(
  address: string,
  onRetrySuccessfully: (ws: WebSocket) => void
): WebSocket {
  const ws = new WebSocket(address);

  ws.addEventListener("close", (event: CloseEvent) => {
    if (event.code === WS_CLOSE_CODE.UNMOUNT) {
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
      { retries: 100, retryIntervalMs: 500 }
    );
  });

  return ws;
}

function keyMessageFactory(data: string) {
  return JSON.stringify(
    keyMessage.parse({
      command: Command.key,
      data,
    })
  );
}
// https://mpv.io/manual/stable/#key-names
export function buildMPVKeyName(event: KeyboardEvent): string {
  return;
}
