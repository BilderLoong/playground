const customEvent = new Event('custom', {
  bubbles: false,
  cancelable: false,
});

const button = document.querySelector('button');
button.addEventListener('click', (e) => {
  console.log(e.isTrusted);
});

button.addEventListener('custom', (e) => {
  console.log(e);
});

button.dispatchEvent(customEvent);

// Event target
(function () {
  const eventTarget = new EventTarget();
  const customEvent = new CustomEvent('sleep', {
    detail: {
      info: 'info',
    },
  });

  eventTarget.addEventListener('sleep', (e) => {
    console.log(e.detail.info);
  });

  eventTarget.dispatchEvent(customEvent);
  console.log(eventTarget);
})();
