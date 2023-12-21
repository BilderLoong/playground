"use server";
import Mpv from "mpv";
import { WebSocketServer } from "ws";
import { incomingMessage, Command } from "../protocols/ws";
import { match, P } from "ts-pattern";
import { resolve } from "node:path";

let _stopMpv: (() => void) | null = null;

export async function stopMpv() {
  _stopMpv?.();
  _stopMpv = null;
}

export async function startMpv({
  videoFileURL,
  subtitleURL,
  WSPort,
}: {
  videoFileURL: string;
  subtitleURL: string;
  WSPort: number;
}) {
  const MPVACIOUS_PATH = resolve("../mpvacious/");
  const mpv = await Mpv({
    path: "mpv",
    args: [
      // "--audio-display=no",
      // "--no-video",
      "--no-audio",
      "--window-minimized=yes",
      `--script=${MPVACIOUS_PATH}`,
    ],
  });

  mpv.command("loadfile", videoFileURL);

  mpv.on("file-loaded", () => {
    mpv.command("sub-add", subtitleURL);
  });

  mpv.observe("sub-text", (subText?: string) => {
    // console.log(subText);
  });

  const wss = new WebSocketServer({ port: WSPort });

  mpv.process?.on("close", () => {
    wss.close();
  });

  wss.on("connection", function connection(ws) {
    ws.on("error", console.error);

    ws.on("message", function message(data) {
      messageDispatcher(data.toString());
    });
  });

  _stopMpv = () => {
    wss.close();
    mpv.process?.kill();
  };

  function messageDispatcher(wsMsg: string) {
    const parsedMsg = incomingMessage.safeParse(JSON.parse(wsMsg));
    if (!parsedMsg.success) {
      return {
        message: `Unsupported message: ${wsMsg}`,
      };
    }

    match(parsedMsg.data).with(
      { command: Command.key, data: { key: P.select("key") } },
      ({ key }) => {
        match(key)
          .with("j", async (key) => {
            mpv.command("script-message", "mpvacious-sub-seek-forward");
          })
          .with("k", async (key) => {
            mpv.command("script-message", "mpvacious-sub-seek-back");
          })
          .with("a", async (key) => {
            mpv.command("script-message", "mpvacious-menu-open");
          })
          .with("c", async (key) => {
            mpv.command("script-message", "mpvacious-copy-sub-to-clipboard");
          });
      }
    );
  }
}
