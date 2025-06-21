import { describe, expect, test } from "bun:test";
import { processBlockContent } from "./pure";

describe("processBlockContent", () => {
  test("initial test", async () => {
    const result = await processBlockContent(
      "This is a test with a URL: https://example.com and some text."
    );

    expect(result).toBe(
      "This is a test with a URL: [Example Domain](https://example.com) and some text."
    );
  });
  
  test("handle URL with parameter", async () => {
    const result = await processBlockContent(
      "This is a test with a URL: https://www.youtube.com/watch?v=SdHE2zgSaxU&t=170s."
    );

    expect(result).toBe(
      "This is a test with a URL: [How To Cook A Perfect Steak Every Time - YouTube](https://www.youtube.com/watch?v=SdHE2zgSaxU&t=170s)."
    );
  });

  test("handle trailing punctuation", async () => {
    const result = await processBlockContent(
      "This is a test with a URL: https://example.com."
    );

    expect(result).toBe(
      "This is a test with a URL: [Example Domain](https://example.com)."
    );
  });
  
  test("handle multiple URLs", async () => {
    const result = await processBlockContent(
      "This is a test with multiple URLs: https://example.com and https://example.org"
    );
    expect(result).toBe(
      "This is a test with multiple URLs: [Example Domain](https://example.com) and [Example Domain](https://example.org)"
    );
  });
  test("handle markdown links", async () => {
    const result = await processBlockContent(
      "This is a test with a markdown link: [Example Domain](https://example.com)."
    );
    expect(result).toBe(
      "This is a test with a markdown link: [Example Domain](https://example.com)."
    );
  });
  test("handle no URLs", async () => {
    const result = await processBlockContent(
      "This is a test with no URLs."
    );
    expect(result).toBe(
      "This is a test with no URLs."
    );
  });

  test("handle partial markdown situation", async () => {
    const result = await processBlockContent(
      `This is a test with a URL: https://example.com and some text.
      [Example Domain](https://example.com) https://example.org`
    );

    expect(result).toBe(
      `This is a test with a URL: [Example Domain](https://example.com) and some text.
      [Example Domain](https://example.com) [Example Domain](https://example.org)`
    );
  });
});
