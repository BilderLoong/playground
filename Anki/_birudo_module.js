/**
 * @typedef {Object} Api
 * @property {function(): void} ankiBuryCard - Une méthode pour enterrer une carte.
 * @property {function(string): void} ankiShowToast - Une méthode pour afficher un toast.
 * @property {function(): number} ankiGetCardFlag - Une méthode pour afficher un toast.
 * @property {function(number): void } ankiToggleFlag - Une méthode pour afficher un toast.
 */
/** @type {Api} */
var api = registerAPI();
if (api) {
  setTimeout(run, 0);
}

async function run() {
  burryAudioOnlyCard(api);
  makeATagOnlyRunOnLongTouch();
  // await loadScript("https://cdn.jsdelivr.net/npm/eruda").then(() => {
  //   eruda.init();
  // });
}

/**
 * @param apiNames {string[]}
 */
function registerAPI() {
  try {
    // To use toggleFlag api need to initialize first: https://github.com/ankidroid/Anki-Android/wiki/AnkiDroid-Javascript-API#initialize
    const api = AnkiDroidJS.init({
      version: "0.0.2",
      developer: "cokoryu@outlook.com",
    });

    console.log({ api });

    return api;
  } catch (error) {
    console.error(error);
  }
}

function burryAudioOnlyCard() {
  const text = document.querySelector("#qa")?.innerText;
  console.log({ 'document.querySelector("#qa")?.innerText;': text });

  if (text) {
    return;
  }

  api.ankiBuryCard();
}

function makeATagOnlyRunOnLongTouch() {
  document.querySelectorAll("a").forEach((a) => {
    a.addEventListener("click", (e) => {
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

function runOnLongTouch(element, func, delay) {
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
  const FLAG_ENUM = {
    NONE: "none",
    BLUE: "blue",
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

  const currentFlag = api.ankiGetCardFlag();

  console.debug({ currentFlag });
  console.debug({
    "currentFlag === FLAG_ENUM.BLUE": currentFlag === FLAG_ENUM.BLUE,
  });
  
  if (currentFlag === FLAG_ENUM.BLUE) {
    console.log("Burry card");
    api.ankiBuryCard();
    return;
  }

  const isNeedScroll = _isNeedScroll();
  console.log({ isNeedScroll });
  if (isNeedScroll) {
    api.ankiToggleFlag(FLAG_ENUM.BLUE);
    api.ankiBuryCard();
    return;
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
