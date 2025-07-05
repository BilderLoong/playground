// ==UserScript==
// @name          Remove Chinese translation and blur english translation on mainichi-nonbiri.com
// @namespace     http://tampermonkey.net/
// @version       1.0.0
// @description   Remove Chinese translation and blur English translation on mainichi-nonbiri.com
// @author        You
// @match         https://mainichi-nonbiri.com/grammar/*
// @icon          https://www.google.com/s2/favicons?sz=64&domain=mainichi-nonbiri.com
// @grant         none
// ==/UserScript==

(function() {
    'use strict';

    const style = document.createElement('style');
    style.textContent = `
        .gray {
            filter: blur(5px); /* Initial blur */
            transition: filter 0.3s ease-in-out; /* Smooth transition */
        }

        .gray {
          display: none;
        }

        .gray:hover {
            filter: blur(0px); /* Unblur on hover */
        }
    `;
    document.head.appendChild(style);

})();