import { watch } from "fs";
import { userScriptBanner } from "./src/userscript_banner";

const distDirName = "dist";
// Define your userscript header as a template literal
// Define the build function
async function buildScript() {
  console.log("Building script...");
  try {
    await Bun.build({
      entrypoints: ["./src/index.user.ts"],
      outdir: `./${distDirName}`,
      banner: userScriptBanner,
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
