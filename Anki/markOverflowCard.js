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
