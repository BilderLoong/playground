markOverflowCard();
function markOverflowCard() {
  // Change to toggle whether show the toast.
  const DEBUG = false;

  if (!_canUseToggleFlag()) {
    toast(`can't use toggle flag`);
    return;
  }

  const BLUE = 4;
  const NONE = 0;

  toast(`Flag: ${AnkiDroidJS.ankiGetCardFlag()}`);
  toast(`_isNeedScroll(): ${_isNeedScroll()}`);

  if (AnkiDroidJS.ankiGetCardFlag() !== BLUE && _isNeedScroll()) {
    // blue flag means this cart has overflow content.
    ankiToggleFlag(BLUE);
    toast(`Add blue flag`);
    ankiShowOptionsMenu();
  }

  if (AnkiDroidJS.ankiGetCardFlag() === BLUE && !_isNeedScroll()) {
    ankiToggleFlag(NONE);
    toast(`Remove blue flag`);
  }

  /**
   * @returns {boolean}
   */
  function _isNeedScroll() {
    const bodyWidth = document.body.clientWidth;
    const bodyScrollWidth = document.body.scrollWidth;
    // const viewPortWidth = document.documentElement.clientWidth;
    // const viewPortScrollWidth = document.documentElement.scrollWidth;

    // toast(`viewPortScrollWidth: ${viewPortScrollWidth}`);
    // toast(`viewPortWidth: ${viewPortWidth}`);
    // toast(`bodyScrollWidth: ${bodyScrollWidth}`);
    // toast(`bodyWidth: ${bodyWidth}`);
    // toast(`window.inner: ${window.innerWidth}`);
    const res = bodyScrollWidth > bodyWidth;
    toast(res);
    return res;
  }

  /**
   * @returns {boolean}
   */
  function _canUseAnkiDroidJS() {
    try {
      AnkiDroidJS;
    } catch (error) {
      return false;
    }

    return true;
  }

  /**
   * @returns {boolean}
   */
  function _canUseToggleFlag() {
    toast(`_canUseToggleFlag run`);
    try {
      // To use toggleFlag api need to initialize first: https://github.com/ankidroid/Anki-Android/wiki/AnkiDroid-Javascript-API#initialize
      const apiStatus = AnkiDroidJS.init(
        JSON.stringify({ version: '0.0.1', developer: 'cokoryu@outlook.com' })
      );

      const api = JSON.parse(apiStatus);

      if (!api['toggleFlag']) {
        toast(`can't use api[toggleFlag]`);
        return false;
      }
    } catch (error) {
      toast(`can't use AnkiDroidJS`);
      return false;
    }

    toast(`able to use AnkiDroidJS`);
    return true;
  }

  /**
   * @param {string} msg
   * @return {void}
   */
  function toast(msg) {
    if (_canUseAnkiDroidJS() && DEBUG) {
      ankiShowToast(msg.toString());
    }
  }
}
setTimeout(markOverflowCard, 0);

function markOverflowCard() {
  if (!resizeDone) {
    setTimeout(markOverflowCard, 0);
  }

  // Change to toggle whether show the toast.
  const DEBUG = false;

  if (!_canUseToggleFlag()) {
    display(`can't use toggle flag`);
    return;
  }

  const BLUE = 4;
  const NONE = 0;

  display(`Flag: ${AnkiDroidJS.ankiGetCardFlag()}`);
  display(`_isNeedScroll(): ${_isNeedScroll()}`);

  if (AnkiDroidJS.ankiGetCardFlag() !== BLUE && _isNeedScroll()) {
    // blue flag means this cart has overflow content.
    ankiToggleFlag(BLUE);
    ankiShowOptionsMenu();
    display(`Added blue flag`);
  }

  if (AnkiDroidJS.ankiGetCardFlag() === BLUE && !_isNeedScroll()) {
    ankiToggleFlag(NONE);
    display(`Remove blue flag`);
  }

  /**
   * @returns {boolean}
   */
  function _isNeedScroll() {
    const bodyWidth = document.body.clientWidth;
    const bodyScrollWidth = document.body.scrollWidth;
    const viewPortWidth = document.documentElement.clientWidth;
    const viewPortScrollWidth = document.documentElement.scrollWidth;

    display(`viewPortScrollWidth: ${viewPortScrollWidth}`);
    display(`viewPortWidth: ${viewPortWidth}`);
    display(`bodyScrollWidth: ${bodyScrollWidth}`);
    display(`bodyWidth: ${bodyWidth}`);
    display(`window.inner: ${window.innerWidth}`);
    const res = bodyScrollWidth - bodyWidth > 100;
    // toast(res);
    return res;
  }

  /**
   * @returns {boolean}
   */
  function _canUseAnkiDroidJS() {
    try {
      AnkiDroidJS;
    } catch (error) {
      return false;
    }

    return true;
  }

  /**
   * @returns {boolean}
   */
  function _canUseToggleFlag() {
    display(`_canUseToggleFlag run`);
    try {
      // To use toggleFlag api need to initialize first: https://github.com/ankidroid/Anki-Android/wiki/AnkiDroid-Javascript-API#initialize
      const apiStatus = AnkiDroidJS.init(
        JSON.stringify({ version: '0.0.1', developer: 'cokoryu@outlook.com' })
      );

      const api = JSON.parse(apiStatus);

      if (!api['toggleFlag']) {
        display(`can't use api[toggleFlag]`);
        return false;
      }
    } catch (error) {
      display(`can't use AnkiDroidJS`);
      return false;
    }

    display(`able to use AnkiDroidJS`);
    return true;
  }

  /**
   * @param {string} msg
   * @return {void}
   */
  function display(msg) {
    if (_canUseAnkiDroidJS() && DEBUG) {
      msg = msg.toString();
      addStrToCard(msg);
    }
  }
}

/**
 * @description add string to the begin of the card for debugging
 * @param {string} str;
 * @return {HTMLElement};
 */
function addStrToCard(str) {
  const card = document.getElementsByClassName('card')[0];
  card.innerHTML = `${str}<br/> ${card.innerHTML}`;
  return card;
}
