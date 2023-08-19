export function runOnLongTouch(element, func, delay) {
  console.log(element);
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

export function openLinks(links) {
  links.forEach((link) => {
    window.open(link, "_blank");
  });
}
