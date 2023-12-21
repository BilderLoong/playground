import React from "react";
import { parse, parseSync } from "subtitle";

export default function SubtitleViewer({ blob }: { blob: Blob }) {
  blob.stream().pipeThrough(parse()).getReader().read("123");
  return <div>SubtitleViewer</div>;
}
