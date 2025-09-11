// ==UserScript==
// @name         Extend.user youtube_subtitle
// @namespace    http://tampermonkey.net/
// @version      1.1
// @description  Automatically log all subtitles of currently selected track in YouTube
// @match        https://www.youtube.com/watch*
// @grant        none
// ==/UserScript==

(function () {
    'use strict';
    console.log("📝 YouTube Subtitles Auto Logger loaded.");

    async function getSubs() {
        // Grab player response
        let ytplayer = window.ytInitialPlayerResponse;
        if (!ytplayer || !ytplayer.captions) {
            console.log("❌ No subtitles available on this video.");
            return;
        }

        let captionTracks = ytplayer.captions.playerCaptionsTracklistRenderer.captionTracks;
        if (!captionTracks || captionTracks.length === 0) {
            console.log("❌ No caption tracks found.");
            return;
        }

        // Pick the first track (usually current UI-selected one)
        let selectedTrack = captionTracks[0];

        console.log("📺 Using subtitle track:", selectedTrack.name.simpleText, selectedTrack.languageCode);

        // Fetch subtitle JSON
        let url = selectedTrack.baseUrl + "&fmt=json3";
        let response = await fetch(url);
        let data = await response.json();

        let texts = [];
        if (data && data.events) {
            for (let event of data.events) {
                if (event.segs) {
                    let line = event.segs.map(seg => seg.utf8).join("");
                    texts.push(line.trim());
                }
            }
        }

        console.log("✅ Full subtitles (" + texts.length + " lines):");
        console.log(texts.join("\n"));
    }

    // Wait until player response is ready, then run once
    let observer = new MutationObserver(() => {
        if (window.ytInitialPlayerResponse) {
            try {
                getSubs();
            } catch (error) {
                console.error("❌ Error fetching subtitles:", error);
            }
            observer.disconnect(); // stop watching after logging once
        }
    });

    observer.observe(document, { childList: true, subtree: true });
})();
