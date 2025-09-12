// ==UserScript==
// @name         XHR Interceptor (Tampermonkey)
// @namespace    http://tampermonkey.net/
// @version      0.2
// @description  Intercept and inspect/modify XMLHttpRequest (open, setRequestHeader, send, responseText).
// @author       You
// @match        *://www.youtube.com/watch*
// @grant        none
// @run-at       document-start
// ==/UserScript==

// @require      https://unpkg.com/ajax-hook@3.0.3/dist/ajaxhook.min.js

/** @type {Map<string, XMLHttpRequest>} */
const subtitleMap = new Map();

(() => {
  /**
   * @param {function(originalSubtitleEle: Element | null): void} callback - Callback function called when a subtitle element is added
   */

  /**
   * @returns {() => string | undefined}
   */
  
})();

(function () {
  "use strict";

  if (window.__XHR_INTERCEPTOR_INSTALLED__) return;
  window.__XHR_INTERCEPTOR_INSTALLED__ = true;

  const origOpen = XMLHttpRequest.prototype.open;
  const origSend = XMLHttpRequest.prototype.send;
  const origSetRequestHeader = XMLHttpRequest.prototype.setRequestHeader;

  // --- user hooks: edit these functions to change behavior ---
  // Called right after open(method, url, ...)
  function onRequestOpen(xhr, method, url, async, user, password) {
    // Example: add a property
    // xhr._myIntercept = 'something';
    // return nothing; just inspect/record
  }

  // Called before send(body) — you can modify body by returning a new body
  // Return { proceed: true/false, newBody: <value> }
  function onRequestSend(xhr, body) {
    // Example: block a request
    // if (xhr._url && xhr._url.includes('block-this')) return { proceed: false };
    // Example: modify body for a specific URL
    // if (xhr._url && xhr._url.includes('/api/echo')) {
    //   const newBody = (body || '') + '&injected=1';
    //   return { proceed: true, newBody };
    // }
    return { proceed: true, newBody: body };
  }

  // Called after the response finishes (readyState === 4). You may return a modified responseText.
  // If you return { modify: true, newResponseText: '...' } we'll try to override responseText/response.
  function onResponseReady(xhr) {
    if (xhr?._url.includes("/timedtext")) {
      const url = new URL(xhr._url);
      const lang = url.searchParams.get("lang");
      if (lang) {
        subtitleMap.set(lang, xhr);
      } else {
        console.error("No language found in /timedtext request.");
      }
    }

    return { modify: false };
  }
  // ----------------------------------------------------------------

  XMLHttpRequest.prototype.open = function (
    method,
    url,
    async = true,
    user,
    password
  ) {
    // store details for later
    try {
      this._method = method;
    } catch (e) {}
    try {
      this._url = url;
    } catch (e) {}
    try {
      this._async = async;
    } catch (e) {}
    try {
      this._openArgs = { method, url, async, user, password };
    } catch (e) {}

    // user hook
    try {
      onRequestOpen(this, method, url, async, user, password);
    } catch (e) {
      console.error("onRequestOpen error", e);
    }

    return origOpen.apply(this, arguments);
  };

  XMLHttpRequest.prototype.setRequestHeader = function (name, value) {
    // capture headers if desired
    try {
      if (!this._reqHeaders) this._reqHeaders = {};
      this._reqHeaders[name] = value;
    } catch (e) {}
    return origSetRequestHeader.apply(this, arguments);
  };

  XMLHttpRequest.prototype.send = function (body) {
    // allow hook to block/modify body
    let decision = { proceed: true, newBody: body };
    try {
      decision = onRequestSend(this, body) || decision;
    } catch (e) {
      console.error("onRequestSend error", e);
    }

    if (!decision.proceed) {
      // block the request: abort and optionally fire events
      try {
        this.abort();
      } catch (e) {}
      // optionally fire a fake error event (so page scripts don't hang)
      try {
        const ev = new Event("error");
        this.dispatchEvent(ev);
      } catch (e) {}
      return;
    }

    // intercept response
    const _this = this;
    const handler = function () {
      if (_this.readyState === 4) {
        // run user hook
        let respDecision = { modify: false };
        try {
          respDecision = onResponseReady(_this) || respDecision;
        } catch (e) {
          console.error("onResponseReady error", e);
        }

        if (respDecision && respDecision.modify) {
          // Try to override responseText and response. Some pages may expect responseXML too.
          try {
            const newText =
              respDecision.newResponseText === undefined
                ? ""
                : String(respDecision.newResponseText);

            // Override responseText
            try {
              Object.defineProperty(_this, "responseText", {
                configurable: true,
                enumerable: true,
                get() {
                  return newText;
                },
              });
            } catch (e) {
              // Some browsers / contexts might not let us redefine. Log but continue.
              console.warn("Could not override responseText:", e);
            }

            // Override response (same as responseText)
            try {
              Object.defineProperty(_this, "response", {
                configurable: true,
                enumerable: true,
                get() {
                  return newText;
                },
              });
            } catch (e) {
              console.warn("Could not override response:", e);
            }

            // If responseType is '' or 'text', these getters cover it.
          } catch (e) {
            console.error("Failed to apply response override", e);
          }
        }
      }
    };

    // add event listener for state change (safe even if some scripts set onreadystatechange themselves)
    this.addEventListener("readystatechange", handler);

    // call original send with possibly modified body
    return origSend.call(this, decision.newBody);
  };

  // convenience: also patch addEventListener so we don't break pages that enumerate XMLHttpRequest properties
  // (not strictly necessary, but keeps behaviour consistent)
  try {
    const proto = XMLHttpRequest.prototype;
    if (!proto.__xhrPatchedToString) {
      proto.__xhrPatchedToString = true;
      const origToString = proto.toString;
      proto.toString = function () {
        try {
          return origToString.call(this);
        } catch (e) {
          return "[object XMLHttpRequest]";
        }
      };
    }
  } catch (e) {
    /* ignore */
  }

  // Simple test logger (uncomment if you want automatic logs)
  // const _log = (msg, ...args) => console.log('[XHR-INT]', msg, ...args);
  // _log('Interceptor installed at', new Date().toISOString());
})();
