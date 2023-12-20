import { z } from "zod";

export enum Command {
  key,
  subtitle,
}

export const keyMessage = z.object({
  command: z.literal(Command.key),
  data: z.object({
    key: z.string(),
  }),
});

export const subtitle = z.object({
  command: z.literal(Command.subtitle),
});

export const incomingMessage = z.union([keyMessage, subtitle]);
export type IncomingMessage = z.infer<typeof incomingMessage>;
