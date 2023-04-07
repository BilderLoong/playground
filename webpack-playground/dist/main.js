(() => {
  "use strict";
  document.body.appendChild(
    (function () {
      const e = document.createElement("div");
      return (
        (e.innerHTML = _.join(["Hello", "webpack"], " ")),
        console.log(2),
        console.log(123),
        e
      );
    })()
  );
})();
