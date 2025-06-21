/**
 * Mock implementation of pure.ts functions for testing
 */
import * as originalModule from "../pure";

// Create a mock version of the module with all the original exports
const mockModule = { ...originalModule };

// Override the fetchUrlTitle function with a mock implementation
mockModule.fetchUrlTitle = async (url: string): Promise<string | null> => {
  // Return "Example Domain" for the specified URLs
  // Handle URLs with or without trailing punctuation
  const cleanUrl = url.replace(/[.,;:!?]+$/, '');
  if (cleanUrl === "https://example.com" || cleanUrl === "https://example.org") {
    return "Example Domain";
  }
  
  // Handle YouTube URL specifically for the test case
  if (cleanUrl === "https://www.youtube.com/watch?v=SdHE2zgSaxU&t=170s") {
    return "How To Cook A Perfect Steak Every Time - YouTube1";
  }
  
  // For other URLs, return null
  return null;
}; 

// We also need to override urlToMarkdown to use our mocked fetchUrlTitle
mockModule.urlToMarkdown = async (url: string): Promise<string> => {
  try {
    const title = await mockModule.fetchUrlTitle(url);
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

export const { 
  processBlockContent, 
  extractUrls, 
  fetchUrlTitle,
  urlToMarkdown
} = mockModule;
