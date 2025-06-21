/**
 * Types for the URL processing domain
 */
type URL = string;
type Title = string;
type MarkdownLink = string;

/**
 * Represents a range in text with start and end positions
 */
type TextRange = {
  readonly start: number;
  readonly end: number;
};

/**
 * Represents a URL match with its position in text
 */
type URLMatch = {
  readonly url: URL;
  readonly start: number;
  readonly end: number;
};

/**
 * Represents a URL with optional trailing punctuation
 */
type ParsedURL = {
  readonly cleanUrl: URL;
  readonly trailingPunctuation: string;
};

/**
 * Represents a side effect that fetches a title
 */
type TitleFetcher = (url: URL) => Promise<Title | null>;

/**
 * Regular expression patterns
 */
const URL_REGEX = /https?:\/\/[^\s),:;]+(?:[./?=&#][^\s),:;]+)*/g;
const MARKDOWN_LINK_REGEX = /\[([^\]]*)\]\(([^)]+)\)/g;
const TRAILING_PUNCTUATION_REGEX = /([.,;:!?]+)$/;
const TITLE_TAG_REGEX = /<title[^>]*>([^<]+)<\/title>/i;
const MARKDOWN_ESCAPE_REGEX = /(\[|\])/g;

/**
 * Process block content to convert URLs to markdown format
 * This is the main pure function that orchestrates the transformation pipeline
 */
export const processBlockContent = (
  content: string,
  fetchTitle: TitleFetcher = fetchUrlTitle
): Promise<string> => {
  if (!content) return Promise.resolve(content);

  // Extract all components from the content
  const markdownRanges = findMarkdownLinkRanges(content);
  const urlMatches = findUrlMatches(content);
  
  // Filter URLs that aren't already in markdown links
  const urlsToProcess = filterUrlsNotInMarkdown(urlMatches, markdownRanges);
  
  if (urlsToProcess.length === 0) return Promise.resolve(content);
  
  // Sort URLs by position in reverse order to avoid offset issues when replacing
  const sortedUrls = sortUrlsByPositionDesc(urlsToProcess);
  
  // Transform the content by replacing URLs with markdown links
  return replaceUrlsWithMarkdown(content, sortedUrls, fetchTitle);
};

/**
 * Find all markdown link ranges in the content
 */
const findMarkdownLinkRanges = (content: string): ReadonlyArray<TextRange> => {
  const matches = Array.from(content.matchAll(MARKDOWN_LINK_REGEX));
  return matches.map(match => ({
    start: match.index!,
    end: match.index! + match[0].length
  }));
};

/**
 * Find all URL matches in the content
 */
const findUrlMatches = (content: string): ReadonlyArray<URLMatch> => {
  const matches = Array.from(content.matchAll(URL_REGEX));
  return matches.map(match => ({
    url: match[0],
    start: match.index!,
    end: match.index! + match[0].length
  }));
};

/**
 * Filter URLs that aren't already part of markdown links
 */
const filterUrlsNotInMarkdown = (
  urls: ReadonlyArray<URLMatch>,
  markdownRanges: ReadonlyArray<TextRange>
): ReadonlyArray<URLMatch> => {
  return urls.filter(({ start, end }) => 
    !markdownRanges.some(range => 
      start >= range.start && end <= range.end
    )
  );
};

/**
 * Sort URLs by position in descending order
 */
const sortUrlsByPositionDesc = (
  urls: ReadonlyArray<URLMatch>
): ReadonlyArray<URLMatch> => {
  return [...urls].sort((a, b) => b.start - a.start);
};

/**
 * Parse a URL to extract any trailing punctuation
 */
const parseUrl = (url: URL): ParsedURL => {
  const punctuationMatch = url.match(TRAILING_PUNCTUATION_REGEX);
  
  if (!punctuationMatch) {
    return {
      cleanUrl: url,
      trailingPunctuation: ""
    };
  }
  
  const trailingPunctuation = punctuationMatch[1];
  const cleanUrl = url.slice(0, -trailingPunctuation.length);
  
  return {
    cleanUrl,
    trailingPunctuation
  };
};

/**
 * Create a markdown link from a title and URL
 */
const createMarkdownLink = (
  title: Title,
  url: URL,
  trailingPunctuation: string = ""
): MarkdownLink => {
  const escapedTitle = title.replace(MARKDOWN_ESCAPE_REGEX, "\\$1");
  return `[${escapedTitle}](${url})${trailingPunctuation}`;
};

/**
 * Replace a single URL with its markdown equivalent
 */
const replaceUrlWithMarkdown = async (
  content: string,
  urlMatch: URLMatch,
  fetchTitle: TitleFetcher
): Promise<string> => {
  const { url, start, end } = urlMatch;
  const { cleanUrl, trailingPunctuation } = parseUrl(url);
  
  const title = await fetchTitle(cleanUrl);
  
  if (!title) return content;
  
  const markdown = createMarkdownLink(title, cleanUrl, trailingPunctuation);
  
  return content.substring(0, start) + markdown + content.substring(end);
};

/**
 * Replace all URLs with markdown links
 * This function handles the sequential async replacements
 */
const replaceUrlsWithMarkdown = async (
  content: string,
  urls: ReadonlyArray<URLMatch>,
  fetchTitle: TitleFetcher
): Promise<string> => {
  // Use reduce to sequentially apply transformations while maintaining immutability
  return urls.reduce(
    async (contentPromise, urlMatch) => {
      const currentContent = await contentPromise;
      return replaceUrlWithMarkdown(currentContent, urlMatch, fetchTitle);
    },
    Promise.resolve(content)
  );
};

/**
 * Fetch the title of a URL - this function contains side effects (network request)
 * It's isolated from the pure functions and can be injected as a dependency
 */
export const fetchUrlTitle = async (url: URL): Promise<Title | null> => {
  try {
    const formattedUrl = url.startsWith("www.") ? `https://${url}` : url;
    
    // Side effect: Network request
    const response = await fetch(formattedUrl, {
      headers: {
        "User-Agent":
          "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/137.0.0.0 Safari/537.36",
      },
    });

    if (!response.ok) {
      return Promise.reject(new Error(`Failed to fetch URL: ${response.status}`));
    }

    const html = await response.text();
    const titleMatch = html.match(TITLE_TAG_REGEX);
    return titleMatch ? titleMatch[1].trim() : null;
  } catch (error) {
    // Side effect: Logging
    console.error(
      `Error fetching URL title: ${
        error instanceof Error ? error.message : String(error)
      }`
    );
    return null;
  }
};
