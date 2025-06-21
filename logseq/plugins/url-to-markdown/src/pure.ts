/**
 * URL regex pattern to detect URLs in text
 * Matches common URL patterns starting with http://, https://, or www.
 * Excludes trailing punctuation like periods, commas, etc.
 * Properly handles query parameters and fragments
 */
const URL_REGEX = /https?:\/\/[^\s),:;]+(?:[./?=&#][^\s),:;]+)*/g;
const MARKDOWN_LINK_REGEX = /\[([^\]]*)\]\(([^)]+)\)/g;

/**
 * Process block content to convert URLs to markdown format
 * @param content - The block content to process
 * @returns The processed content with URLs converted to markdown
 */
/**
 * Custom implementation of processBlockContent for testing
 * This doesn't rely on the original module's functions
 */
export const processBlockContent = async (content: string): Promise<string> => {
  if (!content) return content;
  
  // Find all markdown links to exclude them from processing
  const markdownLinks = [...content.matchAll(MARKDOWN_LINK_REGEX)];
  const markdownRanges = markdownLinks.map((match: RegExpMatchArray) => ({
    start: match.index!,
    end: match.index! + match[0].length
  }));
  
  // Find all URLs
  const matches = [...content.matchAll(URL_REGEX)];
  
  // Filter out URLs that are already part of markdown links
  const urlsToProcess = matches
    .filter((match: RegExpMatchArray) => {
      const start = match.index!;
      const end = start + match[0].length;
      return !markdownRanges.some(range => 
        start >= range.start && end <= range.end
      );
    })
    .map((match: RegExpMatchArray) => ({
      url: match[0],
      start: match.index!,
      end: match.index! + match[0].length
    }));
  
  if (urlsToProcess.length === 0) return content;
  
  // Sort URLs by position in reverse order to avoid offset issues when replacing
  urlsToProcess.sort((a: {start: number}, b: {start: number}) => b.start - a.start);
  
  // Create a mutable copy of the content
  let result = content;
  
  // Process each URL
  for (const { url, start, end } of urlsToProcess) {
    // Check for trailing punctuation
    let trailingPunctuation = '';
    const originalText = result.substring(start, end);
    const punctuationMatch = originalText.match(/([.,;:!?]+)$/);
    
    if (punctuationMatch) {
      trailingPunctuation = punctuationMatch[1];
      // Adjust the URL to exclude the trailing punctuation
      const cleanUrl = url.slice(0, -trailingPunctuation.length);
      
      const title = await fetchUrlTitle(cleanUrl);
      if (title) {
        const markdown = `[${title.replace(/(\[|\])/g, "\\$1")}](${cleanUrl})${trailingPunctuation}`;
        result = result.substring(0, start) + markdown + result.substring(end);
      }
    } else {
      const title = await fetchUrlTitle(url);
      if (title) {
        const markdown = `[${title.replace(/(\[|\])/g, "\\$1")}](${url})`;
        result = result.substring(0, start) + markdown + result.substring(end);
      }
    }
  }
  
  return result;
};

/**
 * Convert a URL to markdown format with its title
 * @param url - The URL to convert
 * @returns The markdown formatted link
 */
export const urlToMarkdown = async (url: string): Promise<string> => {
  try {
    const title = await fetchUrlTitle(url);
    return title ? `[${title.replace(/(\[|\])/g, "\\$1")}](${url})` : url;
  } catch (error) {
    console.error(
      `Error converting URL to markdown: ${
        error instanceof Error ? error.message : String(error)
      }`
    );
    return url;
  }
};

/**
 * Extract all URLs from text content that are not already in a markdown link
 * @param content - The text content to extract URLs from
 * @returns Array of extracted URLs
 */
export const extractUrls = (content: string): string[] => {
  const markdownLinkMatches = [...content.matchAll(MARKDOWN_LINK_REGEX)];
  const markdownRanges = markdownLinkMatches.map((match) => ({
    start: match.index!,
    end: match.index! + match[0].length,
  }));

  const urlMatches = [...content.matchAll(URL_REGEX)];

  const urls = urlMatches
    .filter((match) => {
      const urlStart = match.index!;
      const urlEnd = urlStart + match[0].length;
      return !markdownRanges.some(
        (range) => urlStart >= range.start && urlEnd <= range.end
      );
    })
    .map((match) => match[0]);

  return [...new Set(urls)];
};

interface Replacement {
  original: string;
  replacement: string;
}

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
  return replacements.reduce(
    (updatedContent, { original, replacement }) =>
      updatedContent.replaceAll(original, replacement),
    content
  );
};

/**
 * Fetch the title of a URL
 * This function contains side effects (network request)
 * @param url - The URL to fetch the title from
 * @returns The title of the URL or null if not found
 */
export const fetchUrlTitle = async (url: string): Promise<string | null> => {
  try {
    const formattedUrl = url.startsWith("www.") ? `https://${url}` : url;
    const response = await fetch(formattedUrl, {
      headers: { "User-Agent": "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/137.0.0.0 Safari/537.36" },
    });

    if (!response.ok) {
      throw new Error(`Failed to fetch URL: ${response.status}`);
    }

    const html = await response.text();
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
