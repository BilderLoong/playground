import { proxy } from "./interceptor";

/** @type {Map<string, XMLHttpRequest>} */
const subtitleMap = new Map();

(() => {
  proxy({
    onResponseReady: (xhr) => {
      if (xhr?._url.includes("/timedtext")) {
        const url = new URL(xhr._url);
        const lang = url.searchParams.get("lang");
        if (lang) {
          subtitleMap.set(lang, xhr);
          console.log(`Subtitle fetched for lang: ${lang}`);
        } else {
          console.error("No language found in /timedtext request.");
        }
      }

      return { modify: false };
    },
  });
})();
