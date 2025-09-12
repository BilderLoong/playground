import { watch } from "fs";

const distDirName = 'dist'
// Define your userscript header as a template literal
const userscriptHeader = `
// ==UserScript==
// @name         Language reactor subtitle extender
// @namespace    http://tampermonkey.net/
// @version      1.1
// @description  So that Yomitan (or other popup dictionary) can pick up full sentence.
// @author       Birudo
// @match        *://www.youtube.com/watch*
// @grant        none
// @run-at       document-start
// @updateURL    https://raw.githubusercontent.com/BilderLoong/playground/refs/heads/master/tampermonkey_scripts/Youtube_subtitle_extend/${distDirName}/index.user.js
// @downloadURL  https://raw.githubusercontent.com/BilderLoong/playground/refs/heads/master/tampermonkey_scripts/Youtube_subtitle_extend/${distDirName}/index.user.js
// ==/UserScript==
`.trim(); // .trim() removes any leading/trailing whitespace

// Define the build function
async function buildScript() {
  console.log("Building script...");
  try {
    await Bun.build({
      entrypoints: ["./src/index.user.ts"],
      outdir: `./${distDirName}`,
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
