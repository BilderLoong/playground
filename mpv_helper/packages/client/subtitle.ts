import { parse, Node } from "subtitle";
import { createReadStream } from "node:fs";

const readableStream = createReadStream("001.srt", { encoding: "utf-8" });
readableStream
  .pipe(parse())
  .on("data", (node: Node) => {
    console.log("parsed node:", node);
  })
  .on("error", console.error)
  .on("finish", () => console.log("parser has finished"));
