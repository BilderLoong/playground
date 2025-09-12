import { watch } from "fs";

// Define your userscript header as a template literal
const userscriptHeader = `
// ==UserScript==
// @name         XHR Interceptor (Tampermonkey)
// @namespace    http://tampermonkey.net/
// @version      0.2
// @description  Intercept and inspect/modify XMLHttpRequest (open, setRequestHeader, send, responseText).
// @author       You
// @match        *://www.youtube.com/watch*
// @grant        none
// @run-at       document-start
// ==/UserScript==
`.trim(); // .trim() removes any leading/trailing whitespace

// Define the build function
async function buildScript() {
  console.log("Building script...");
  try {
    await Bun.build({
      entrypoints: ["./src/index.user.ts"],
      outdir: "./dist",
      banner: userscriptHeader,
    });
    console.log("✅ Build successful!");
  } catch (error) {
    console.error("❌ Build failed:", error);
  }
}

// 1. Run the initial build immediately
buildScript();

// 2. Watch the 'src' directory for any changes
console.log("👀 Watching for changes in ./src");
watch("./src", { recursive: true }, (eventType, filename) => {
  if (filename) {
    console.log(`\nFile changed: ${filename}`);
    // Re-run the build function on change
    buildScript();
  }
});
