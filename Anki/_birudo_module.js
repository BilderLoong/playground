const api = registerAPI();
if (api) {
  setTimeout(run, 0);
}

async function run() {
  makeATagOnlyRunOnLongTouch();
  // await loadScript("https://cdn.jsdelivr.net/npm/eruda").then(() => {
  //   eruda.init();
  // });
}

function registerAPI() {
  try {
    // To use toggleFlag api need to initialize first: https://github.com/ankidroid/Anki-Android/wiki/AnkiDroid-Javascript-API#initialize
    const api = new AnkiDroidJS({
      version: "0.0.3",
      developer: "cokoryu@outlook.com",
    });

    console.log({ api });

    return api;
  } catch (error) {
    console.error(error);
  }
}

// function burryAudioOnlyCard() {
//   const text = document.querySelector("#qa")?.innerText;
//   console.log({ 'document.querySelector("#qa")?.innerText;': text });
//
//   if (text) {
//     return;
//   }
//
//   api?.ankiBuryCard();
// }

function makeATagOnlyRunOnLongTouch() {
  document.querySelectorAll("a").forEach((a) => {
    a.addEventListener("pointerdown", (e) => {
      if (e.pointerType === "touch") {
        e.preventDefault();
      }
    });

    runOnLongTouch(
      a,
      () => {
        a.click();
      },
      1500,
    );
  });
}

/**
 * Runs a function when a long touch is detected on the specified element.
 *
 * @param {HTMLElement} element - The element to listen for long touch events.
 * @param {Function} func - The function to be executed when a long touch is detected.
 * @param {number} delay - The delay in milliseconds that determines when a touch is considered a long touch.
 */
function runOnLongTouch(element, func, delay) {
  /** @type {number | undefined} */
  let pressTimer;

  element.addEventListener("touchstart", function () {
    // Start the timer
    pressTimer = window.setTimeout(func, delay);
  });

  element.addEventListener("touchend", function () {
    // If the timer is still running, clear it
    if (pressTimer) window.clearTimeout(pressTimer);
  });
}

export async function burryAndMarkOverflowCard() {
  if (!api) {
    return;
  }

  const FLAG_ENUM = {
    NONE: 0,
    BLUE: 4,
  };
  try {
    // resizeDone is undefined in the desktop version of Anki.
    if (!resizeDone) {
      setTimeout(burryAndMarkOverflowCard, 100);
      return;
    }
  } catch (err) {
    return;
  }

  const { value: currentFlag } = await api.ankiGetCardFlag();

  console.debug({ currentFlag });

  if (currentFlag === FLAG_ENUM.BLUE) {
    api.ankiBuryCard();
    return;
  }

  const isNeedScroll = _isNeedScroll();
  if (isNeedScroll) {
    return await Promise.all([api.ankiToggleFlag("blue"), api.ankiBuryCard()]);
  }

  /**
   * @returns {boolean}
   */
  function _isNeedScroll() {
    const bodyWidth = document.body.clientWidth;
    const bodyScrollWidth = document.body.scrollWidth;
    const viewPortWidth = document.documentElement.clientWidth;
    const viewPortScrollWidth = document.documentElement.scrollWidth;

    const res = bodyScrollWidth - bodyWidth > 100;
    return res;
  }
}
