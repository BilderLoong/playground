console.log(a);

if (a) {
  var a = 1;
}

console.log(a);

function a() {
  console.log(this);
}

console.log(a);

a();
try {
  
} catch (error) {
  
}
try {
  console.log(Symbol.for('id') === Symbol.for('id'));
  console.log(Symbol.for('id') === Symbol('id'));
} catch (error) {}
try {
  const bar = {
    name: 1,
    age: undefined,
  };

  console.log({ ...bar, name: 2 });
} catch (error) {}
try {
  function foo({ name, age } = {}) {
    const obj = {
      name,
      age,
    };

    console.log({ ...obj });
  }

  foo({ name: 1, height: 4 });
  // foo({ name: 1, age: 4 });
} catch (error) {}
try {
  markOverflowCard();
  function markOverflowCard() {
    ankiShowToast(`markOverflowCard run`);
    if (!_canUseToggleFlag()) {
      ankiShowToast(`can't use toggle flag`);
      return;
    }

    const BLUE = 4;
    const NONE = 0;

    if (AnkiDroidJS.ankiGetCardFlag() !== BLUE && _isNeedScroll()) {
      // blue flag means this cart has overflow content.
      ankiToggleFlag(BLUE);
      ankiShowToast(`Add blue flag`);
    }

    if (AnkiDroidJS.ankiGetCardFlag() === BLUE && !_isNeedScroll()) {
      ankiToggleFlag(NONE);
      ankiShowToast(`Remove blue flag`);
    }

    /**
     * @returns {boolean}
     */
    function _isNeedScroll() {
      const bodyWidth = document.body.clientWidth;
      const viewPortWidth = document.documentElement.clientWidth;

      return bodyWidth > viewPortWidth;
    }

    /**
     *
     * @returns {boolean}
     */
    function _canUseToggleFlag() {
      ankiShowToast(`_canUseToggleFlag run`);
      try {
        // To use toggleFlag api need to initialize first: https://github.com/ankidroid/Anki-Android/wiki/AnkiDroid-Javascript-API#initialize
        const apiStatus = AnkiDroidJS.init(
          JSON.stringify({ version: '0.0.1', developer: 'cokoryu@outlook.com' })
        );

        const api = JSON.parse(apiStatus);

        if (!api['toggleFlag']) {
          ankiShowToast(`can't use api[toggleFlag]`);
          return false;
        }
      } catch (error) {
        ankiShowToast(error);
        ankiShowToast(`can't use AnkiDroidJS`);
        return false;
      }
    }

    ankiShowToast(`able to use AnkiDroidJS`);
    return true;
  }
} catch (error) {}
