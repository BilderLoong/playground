// ==UserScript==
// @name         Simplify Xiaohongshu
// @namespace    http://tampermonkey.net/
// @updateURL    https://raw.githubusercontent.com/BilderLoong/playground/refs/heads/master/tampermonkey_scripts/Simpify_Xiaohongshu.js
// @downloadURL  https://raw.githubusercontent.com/BilderLoong/playground/refs/heads/master/tampermonkey_scripts/Simpify_Xiaohongshu.js
// @version      1.0.0
// @description  Remove the feed page on Xiaohongshu.com;
// @author       cokoryu@outlook.com
// @match        https://www.xiaohongshu.com/explore/*
// @grant        none
// @license MIT
// ==/UserScript==

(function() {
    'use strict';
     // Select the element containing the watermark using its CSS class
    const style = document.createElement('style');
    style.innerHTML = `
    #exploreFeeds, #channel-container {
       display: none !important;
    }
    `;
    document.head.appendChild(style);
})();