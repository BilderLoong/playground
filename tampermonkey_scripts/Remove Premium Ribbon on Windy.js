// ==UserScript==
// @name         Remove Premium Ribbon on Windy.com
// @namespace    http://tampermonkey.net/
// @updateURL    https://raw.githubusercontent.com/BilderLoong/playground/refs/heads/master/tampermonkey_scripts/Remove%20Premium%20Ribbon%20on%20Windy.js
// @downloadURL  https://raw.githubusercontent.com/BilderLoong/playground/refs/heads/master/tampermonkey_scripts/Remove%20Premium%20Ribbon%20on%20Windy.js
// @version      1.2.0
// @description  Removes the "Get Premium" ribbon/element on Windy.com
// @author       cokoryu@outlook.com
// @match        https://www.windy.com/*
// @icon         data:image/gif;base64,R0lGODlhAQABAAAAACH5BAEKAAEALAAAAAABAAEAAAICTAEAOw==
// @grant        none
// @license MIT
// ==/UserScript==

(function() {
    'use strict';
     // Select the element containing the watermark using its CSS class
    const style = document.createElement('style');
    style.innerHTML = `
        .get-premium {
            display: none  !important;
        }

        .premium-calendar #map-container::after {
            background-image: none !important;
        }

        .premium-calendar #map-container #leaflet-map {
           filter:none !important;
        }
    `;
    document.head.appendChild(style);
})();