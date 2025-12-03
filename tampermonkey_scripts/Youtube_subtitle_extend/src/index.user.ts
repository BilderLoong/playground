import { proxy } from "./interceptor";

const subtitleMap = new Map<string, XMLHttpRequest>();

(() => {
  proxy({
    onResponseReady: (xhr) => {
      if (!xhr.responseURL.includes("/timedtext")) {
        return { modify: false };
      }

      const url = new URL(xhr.responseURL);
      const lang = url.searchParams.get("lang");
      /** When `tlang` and `lang` both appear the `tlang` is actual language which may be a translated subtitle. fm0 */
      const tlang = url.searchParams.get("tlang");

      if (lang) {
        subtitleMap.set(tlang ?? lang, xhr);
        // console.log({
        //   lang,
        //   xhr,
        // });
      } else {
        console.error("No language found in /timedtext request.");
      }

      return { modify: false };
    },
  });

  /**
   * Waits for an element to exist, then observes it for any changes.
   * @param {string} selector - The CSS selector to watch.
   * @param {function} callback - The function to run when changes happen.
   */
  function observeSelector(
    selector: string,
    callback: (mutations: MutationRecord[], element: Element) => void
  ) {
    // 1. Define the logic to start watching the specific element
    function startObserving(element: Element) {
      const observer = new MutationObserver((mutations) => {
        // Run the user's callback whenever a change is detected
        callback(mutations, element);
      });

      observer.observe(element, {
        // attributes: true, // Watch attribute changes
        childList: true, // Watch add/remove of children
        subtree: true, // Watch deep inside the element
        characterData: true, // Watch text changes
      });

      console.log(`Now observing: ${selector}`);
    }

    // 2. Check if the element already exists
    const existingElement = document.querySelector(selector);
    if (existingElement) {
      startObserving(existingElement);
      return; // Exit, we are done
    }

    // 3. If not, watch the body until the element appears
    const bodyObserver = new MutationObserver((mutations, observer) => {
      const element = document.querySelector(selector);

      if (element) {
        // Element found! Start watching it specifically
        startObserving(element);

        // Stop watching the whole body (performance)
        observer.disconnect();
      }
    });

    bodyObserver.observe(document.body, {
      childList: true,
      subtree: true,
    });
  }

  const getCurrentSubtitleLanguage = getCurrentSubtitleLanguageFactory();
  const getPlayerInstance = getPlayerInstanceFactory();

  function addSubtitleExtensionElements() {
    if (document.querySelector(".subtitle-extension-before")) {
      // Already added
      return;
    }
    const originalSubtitleEle = document.querySelector("#lln-subs");

    if (!originalSubtitleEle) {
      console.error(`No #lln-subs found`);

      return;
    }

    const currentSubtitleLanguage = getCurrentSubtitleLanguage();
    if (!currentSubtitleLanguage) {
      console.error(
        `Got empty, null or undefined language code from body attribute: "lln-sublangcode_g", current languageCode: ${currentSubtitleLanguage}. `
      );
      return;
    }

    console.log(`Current language: ${currentSubtitleLanguage}`);

    function findClosestLanguageCode(target: string): string | null {
      const subtitleMapKeys = Array.from(subtitleMap.keys());

      if (subtitleMapKeys.includes(target)) {
        return target;
      }

      return (
        subtitleMapKeys
          .filter((key) => {
            if (key.includes(target)) {
              return true;
            }
          })
          .sort((a, b) => a.length - b.length)[0] || null
      );
    }

    const closestLanguageCode = findClosestLanguageCode(
      currentSubtitleLanguage
    );
    if (!closestLanguageCode) {
      console.error(
        `No closest language code found in the subtitleMap for current language: ${currentSubtitleLanguage}. `
      );

      return;
    }

    console.log(`Current language **code**: ${closestLanguageCode}`);
    const targetTimedTextRes = subtitleMap.get(closestLanguageCode);

    if (!targetTimedTextRes) {
      console.error(
        `No corresponded XHR found for current language code: ${currentSubtitleLanguage}.`
      );

      return;
    }

    const player = getPlayerInstance();
    if (!player) {
      console.error("No player instance found;");
      return;
    }

    interface TimedSegment {
      tStartMs: number;
      dDurationMs: number;
      segs: [
        {
          utf8: string;
        }
      ];
    }
    interface TimedText {
      events: TimedSegment[];
    }

    const timedTextData = (() => {
      try {
        return JSON.parse(targetTimedTextRes.responseText) as TimedText;
      } catch (error) {
        console.error("Error when parser timedText response to JSON.");
      }
    })();

    if (!timedTextData) {
      console.error("timedText response is empty.");
      return;
    }

    const currentVideoMs = player.getCurrentTime() * 1000;
    const currentSegIndex = timedTextData.events.findIndex(
      ({ dDurationMs: durationMs, tStartMs: startMs }) =>
        currentVideoMs >= startMs && currentVideoMs <= startMs + durationMs
    );

    const { afterSegments, beforeSegments } = timedTextData.events.reduce(
      (acc, current) => {
        if (current.tStartMs + current.dDurationMs < currentVideoMs) {
          return {
            ...acc,
            beforeSegments: [...acc.beforeSegments, current],
          };
        }

        if (current.tStartMs > currentVideoMs) {
          return {
            ...acc,
            afterSegments: [...acc.afterSegments, current],
          };
        }

        return acc;
      },
      {
        beforeSegments: [] as TimedSegment[],
        afterSegments: [] as TimedSegment[],
      }
    );

    // console.log({
    //   currentVideoMs,
    //   beforeSegments,
    //   afterSegments,
    //   all: timedTextData.events,
    // });

    const joinTimedText = (segments: TimedSegment[]): string =>
      segments.map((e) => e.segs[0].utf8).join(" ");

    const beforeText = joinTimedText(beforeSegments);
    const afterText = joinTimedText(afterSegments);

    function hideElement(ele: HTMLElement) {
      ele.style.display = "inline-block";
      ele.style.width = "0";
      ele.style.height = "0";
      ele.style.overflow = "hidden";
    }

    // console.log({
    //   beforeText,
    //   afterText,
    // });

    // console.log({ allText });
    // Create span element before
    const spanBefore = document.createElement("span");
    spanBefore.className = "subtitle-extension-before";
    spanBefore.textContent = beforeText + (beforeText.endsWith(" ") ? "" : " "); // Placeholder content, can be modified as needed
    hideElement(spanBefore);

    // Create span element after
    const spanAfter = document.createElement("span");
    spanAfter.className = "subtitle-extension-after";
    spanAfter.textContent = (afterText.startsWith(" ") ? "" : " ") + afterText; // Placeholder content, can be modified as needed
    hideElement(spanAfter);

    // Insert the spans as second and second-to-last direct child of the `originalSubtitleEle`
    // Insert spanBefore as second child (after the first child if exists)
    const firstChild = originalSubtitleEle.firstChild;
    if (firstChild && firstChild.nextSibling) {
      originalSubtitleEle.insertBefore(spanBefore, firstChild.nextSibling);
    } else if (firstChild) {
      originalSubtitleEle.appendChild(spanBefore);
    } else {
      originalSubtitleEle.appendChild(spanBefore);
    }

    // Insert spanAfter as second-to-last child (before the last child if there's a previous sibling)
    const lastChild = originalSubtitleEle.lastChild;
    if (lastChild && lastChild.previousSibling) {
      originalSubtitleEle.insertBefore(spanAfter, lastChild);
    } else {
      originalSubtitleEle.appendChild(spanAfter);
    }

    // console.log("✅ Added span elements around subtitle");
  }
  observeSelector("#lln-main-subs", () => {
    addSubtitleExtensionElements();
  });
})();

function getCurrentSubtitleLanguageFactory() {
  // const player = document.getElementById("movie_player");
  const body = document.body;

  return () => {
    return body.getAttribute("lln-sublangcode_g");
  };
}

function getPlayerInstanceFactory() {
  let player = document.getElementById("movie_player");

  return function () {
    if (!player) {
      player = document.getElementById("movie_player");
    }

    return player;
  };
}
