/**
 * URL to Markdown - Logseq Plugin
 * Converts URLs in blocks to markdown format with the title of the URL
 */

import "@logseq/libs";
import { processBlockContent } from "./pure";

/**
 * Main plugin initialization
 * This is the entry point and contains side effects (registering commands, UI interactions)
 */ 
const main = async (): Promise<void> => {
  // Register the plugin with Logseq
  logseq.ready().then(initPlugin); 
  // logseq.UI.showMsg("URL to Markdown plugin loaded", "info");
};

/**
 * Initialize the plugin by registering commands and UI elements
 * Side effects are isolated to this function
 */
const initPlugin = (): void => {
  // Register the slash command
  logseq.Editor.registerSlashCommand(
    "Convert URLs to Markdown",
    convertSelectedBlocks
  );

  // Add a toolbar button
  logseq.App.registerUIItem("toolbar", {
    key: "url-to-markdown",
    template:
      '<a style="font-size: 0.7rem;" data-on-click="convertSelectedBlocks" class="button">URL→MD</a>',
  });

  // Register the UI action handler
  logseq.provideModel({
    convertSelectedBlocks,
  });

  // Log that the plugin has been loaded
  console.log("URL to Markdown plugin loaded");
};

async function convertSelectedBlocks() {
  // NOTE: getSelectedBlocks always returns `null` something is wrong.
  const selectedBlocks = await logseq.Editor.getSelectedBlocks();
  const blocksToProcess =
    selectedBlocks && selectedBlocks.length > 0
      ? selectedBlocks
      : [await logseq.Editor.getCurrentBlock()].filter(Boolean);

  if (blocksToProcess.length === 0) return;

  await Promise.all(
    blocksToProcess.map(async (block) => {
      if (!block) return; 

      const updatedContent = await processBlockContent(block.content);

      if (updatedContent === block.content) {
        return;
      }

      await logseq.Editor.updateBlock(block.uuid, updatedContent);
    })
  );
}

// Initialize the plugin
logseq.ready(main).catch((error) => console.error(error));
