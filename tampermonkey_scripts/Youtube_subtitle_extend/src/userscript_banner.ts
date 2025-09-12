export const userScriptBanner = `
// ==UserScript==
// @name         Language reactor subtitle extender
// @namespace    http://tampermonkey.net/
// @version      1.7
// @license MIT
// @description  So that Yomitan (or other popup dictionary) can pick up full sentence.
// @author       Birudo
// @match        *://www.youtube.com/watch*
// @grant        none
// @run-at       document-body
// @updateURL    https://raw.githubusercontent.com/BilderLoong/playground/refs/heads/master/tampermonkey_scripts/Youtube_subtitle_extend/dist/index.user.js
// @downloadURL  https://raw.githubusercontent.com/BilderLoong/playground/refs/heads/master/tampermonkey_scripts/Youtube_subtitle_extend/dist/index.user.js
// ==/UserScript==
`.trim();
