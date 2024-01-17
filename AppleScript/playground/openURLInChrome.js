/**
 * @param {string} input
 * @param {string=} parameters
 */
function run(input, parameters) {
  openYomichanFR(input[0]);

  /**
   * @param {string} query
   */
  function openYomichanFR(query) {
    if (query?.length === 0) {
      return;
    }

    const baseUrl =
      "chrome-extension://fjdmmdclfnmhjmdenccojaghpiachnkk/search.html";

    var url = `${baseUrl}?query=${encodeURIComponent(query)}`;

    openURLInChrome({
      url,
      isUrlReuseable: (url) => url.startsWith(baseUrl),
    });
  }

  /**
   * Ouvre une URL dans Google Chrome.
   *
   * @param {Object} options - Les options pour ouvrir l'URL.
   * @param {string} options.url - L'URL à ouvrir.
   * @param {(url: string) => boolean} options.isUrlReuseable
   */
  function openURLInChrome({ url, isUrlReuseable }) {
    // Create an instance of the Chrome application
    var chrome = Application("Google Chrome");

    // Make sure Chrome is running
    chrome.activate();

    var window, tab, tabIndex, windowIndex;
    // Loop through each window and their tabs to find if the URL is already open
    chrome.windows().forEach((w, wI) => {
      windowIndex = wI;
      w.tabs().forEach((t, i) => {
        if (isUrlReuseable(t.url())) {
          tab = t;
          window = w;
          tabIndex = i + 1;
        }
      });
    });

    if (tab && window) {
      if (tab.url() !== url) {
        tab.url = url;
      }

      // If URL is found, select the tab and exit the loop
      window.activeTabIndex = tabIndex;
      window.index = windowIndex;
      return;
    }

    // If the URL is not already open, then create a new tab with the specified URL
    if (!tab) {
      if (chrome.windows.length === 0) {
        chrome.Window().make(); // No open windows, so make a new one
      }

      // Access the first window and create a new tab with the specified URL
      var firstWindow = chrome.windows[0]; // Index 0 is the first window
      var newTab = chrome.Tab({ url: url });
      firstWindow.tabs.push(newTab);

      // Select the new tab and bring the window to the foreground
      firstWindow.activeTabIndex = firstWindow.tabs.length;
    }
  }
}
