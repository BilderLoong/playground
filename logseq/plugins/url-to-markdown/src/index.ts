/**
 * URL to Markdown - Logseq Plugin
 * Converts URLs in blocks to markdown format with the title of the URL
 */

// Import required libraries
import "@logseq/libs";

interface Replacement {
  original: string;
  replacement: string;
}

/**
 * URL regex pattern to detect URLs in text
 * Matches common URL patterns starting with http://, https://, or www.
 */
const URL_REGEX = /https?:\/\/[^\s)]+|www\.[^\s)]+/g;

/**
 * Main plugin initialization
 * This is the entry point and contains side effects (registering commands, UI interactions)
 */
const main = async (): Promise<void> => {
  // Register the plugin with Logseq
    logseq.ready().then(initPlugin);
};

/**
 * Initialize the plugin by registering commands and UI elements
 * Side effects are isolated to this function
 */
const initPlugin = (): void => {
  // Register the slash command
  logseq.Editor.registerSlashCommand("Convert URLs to Markdown", async () => {
    const currentBlock = await logseq.Editor.getCurrentBlock();
    if (!currentBlock) return;

    // Process the block content and update it
    const updatedContent = await processBlockContent(currentBlock.content);

    // Only update if content changed
    if (updatedContent !== currentBlock.content) {
      logseq.Editor.updateBlock(currentBlock.uuid, updatedContent);
    }
  });

  // Add a toolbar button
  logseq.App.registerUIItem("toolbar", {
    key: "url-to-markdown",
    template:
      '<a data-on-click="convertCurrentBlock" class="button">URL→MD</a>',
  });

  // Register the UI action handler
  logseq.provideModel({
    convertCurrentBlock: async () => {
      const currentBlock = await logseq.Editor.getCurrentBlock();
      if (!currentBlock) return;

      const updatedContent = await processBlockContent(currentBlock.content);

      if (updatedContent !== currentBlock.content) {
        logseq.Editor.updateBlock(currentBlock.uuid, updatedContent);
      }
    },
  });

  // Log that the plugin has been loaded
  console.log("URL to Markdown plugin loaded");
};

/**
 * Process block content to convert URLs to markdown format
 * @param content - The block content to process
 * @returns The processed content with URLs converted to markdown
 */
const processBlockContent = async (content: string): Promise<string> => {
  // If no content or no URLs, return the original content
  if (!content || !URL_REGEX.test(content)) return content;

  // Extract all URLs from the content
  const urls = extractUrls(content);

  // Process each URL and get its replacement
  const replacements = await Promise.all(
    urls.map(async (url) => ({
      original: url,
      replacement: await urlToMarkdown(url),
    }))
  );

  // Apply all replacements to the content
  return applyReplacements(content, replacements);
};

/**
 * Extract all URLs from text content
 * @param content - The text content to extract URLs from
 * @returns Array of extracted URLs
 */
const extractUrls = (content: string): string[] => {
  const matches = content.match(URL_REGEX);
  return matches ? [...new Set(matches)] : []; // Remove duplicates
};

/**
 * Convert a URL to markdown format with its title
 * @param url - The URL to convert
 * @returns The markdown formatted link
 */
const urlToMarkdown = async (url: string): Promise<string> => {
  try {
    const title = await fetchUrlTitle(url);
    // If title is successfully fetched, return markdown format
    return title ? `[${title}](${url})` : url;
  } catch (error) {
    console.error(
      `Error converting URL to markdown: ${
        error instanceof Error ? error.message : String(error)
      }`
    );
    return url; // Return original URL if there's an error
  }
};

/**
 * Fetch the title of a URL
 * This function contains side effects (network request)
 * @param url - The URL to fetch the title from
 * @returns The title of the URL or null if not found
 */
const fetchUrlTitle = async (url: string): Promise<string | null> => {
  try {
    // Ensure URL has proper protocol
    const formattedUrl = url.startsWith("www.") ? `https://${url}` : url;

    // Fetch the URL content
    const response = await fetch(formattedUrl, {
      headers: { "User-Agent": "Logseq URL to Markdown Plugin" },
    });

    if (!response.ok) {
      throw new Error(`Failed to fetch URL: ${response.status}`);
    }

    const html = await response.text();

    // Extract title using regex
    const titleMatch = html.match(/<title[^>]*>([^<]+)<\/title>/i);
    return titleMatch ? titleMatch[1].trim() : null;
  } catch (error) {
    console.error(
      `Error fetching URL title: ${
        error instanceof Error ? error.message : String(error)
      }`
    );
    return null;
  }
};

/**
 * Apply a list of replacements to content
 * @param content - The original content
 * @param replacements - List of replacements
 * @returns The content with replacements applied
 */
const applyReplacements = (
  content: string,
  replacements: Replacement[]
): string => {
  // Reduce over the replacements, applying each one to the content
  return replacements.reduce(
    (updatedContent, { original, replacement }) =>
      updatedContent.replace(original, replacement),
    content
  );
};

// Initialize the plugin
logseq.ready(main).catch((error) => console.error(error));
