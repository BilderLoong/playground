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

  function onLlnSubsWrapAdd(callback: (llnSubsWrap: Element) => void) {
    const observer = new MutationObserver((mutationsList, observer) => {
      // console.log("MutationObserver detected changes:", mutationsList);
      mutationsList.forEach((mutation) => {
        // if (mutation.type !== "childList") return;
        mutation.addedNodes.forEach((node) => {
          if (
            node.nodeType !== Node.ELEMENT_NODE ||
            !(node instanceof Element) ||
            !node.matches(".lln-subs-wrap")
          ) {
            return;
          }

          callback(node);
        });
      });
    });

    observer.observe(document.body, {
      childList: true,
      subtree: true,
      characterData: true, // Observe changes to the text content of nodes
    });
  }

  const getCurrentSubtitleLanguage = getCurrentSubtitleLanguageFactory();
  const getPlayerInstance = getPlayerInstanceFactory();

  onLlnSubsWrapAdd((llnSubsWrap) => {
    const originalSubtitleEle = llnSubsWrap.querySelector("#lln-subs");

    if (!originalSubtitleEle) {
      console.error(`No #lln-subs found`);

      return;
    }

    const currentSubtitleLanguageCode = getCurrentSubtitleLanguage();
    if (!currentSubtitleLanguageCode) {
      console.error(
        `Got empty or undefined language code from player instance, languageCode: ${currentSubtitleLanguageCode}. `
      );
      return;
    }

    // console.log(`Current language code: ${currentSubtitleLanguageCode}`);

    const targetTimedTextRes = subtitleMap.get(currentSubtitleLanguageCode);
    if (!targetTimedTextRes) {
      console.error(
        `No corresponded XHR found for current language code: ${currentSubtitleLanguageCode}.`
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
    spanBefore.textContent = beforeText; // Placeholder content, can be modified as needed
    hideElement(spanBefore);

    // Create span element after
    const spanAfter = document.createElement("span");
    spanAfter.className = "subtitle-extension-after";
    spanAfter.textContent = " " + afterText; // Placeholder content, can be modified as needed
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
  });
})();

function getCurrentSubtitleLanguageFactory() {
  const player = document.getElementById("movie_player");

  return () => {
    if (!player) {
      console.error("no player instance found.");
      return;
    }

    try {
      // There is also a attribute in the `body.lln-sublangcode_g="fr"` may indicate the language of
      // the current selected.
      const {
        Y,
        N,
      }: {
        Y: { languageCode: string } | null;
        N: { languageCode: string } | null;
      } = player.getAudioTrack();

      return Y?.languageCode ?? N?.languageCode;
    } catch (error) {
      console.error(
        "error when getting current subtitle language from player instance.",
        error
      );
    }
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
