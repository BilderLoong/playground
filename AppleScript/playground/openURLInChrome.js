

/**
 * @param {string} input
 * @param {string=} parameters
 */
function run(input, parameters) {
  const app = Application.currentApplication();
  app.includeStandardAdditions = true;
  try {
    // Read the file content
    const text = input[0]
    if (!text) {
      notifyUserWithAlert(`Input text is empty.`);
      return
    }
    try {
      openYomitan(text);
    } catch (error) {
      notifyUserWithAlert(`Error when openYomitan
        Error message:
      ${error}`);
    }

  } catch (error) {
    notifyUserWithAlert(`a oh got an error, so sad.
      Error message:
        ${error}`
    );
  }


  /**
   * @param {string} query
   */
  function openYomitan(query) {
    const baseUrl =
      "chrome-extension://glnaenfapkkecknnmginabpmgkenenml/search.html";
    console.log("query: ", query)
    const url = `${baseUrl}?query=${encodeURIComponent(query)}`;
    console.log("url: ", url)
    openURLInChrome({
      url,
      isUrlReuseable: (url) => url.startsWith(baseUrl),
    });
  }

}



/**
 * Open a URL in Google Chrome, reusing tabs if possible.
 *
 * @param {Object} options - Configuration for opening the URL.
 * @param {string} options.url - The URL to open.
 * @param {(url: string) => boolean} options.isUrlReuseable - Function to check if a tab is reusable.
 */
function openURLInChrome({ url, isUrlReuseable }) {
  const chrome = Application("Google Chrome");
  chrome.activate();

  const allWindows = chrome.windows();
  const reusableTab = findReusableTab(allWindows, isUrlReuseable);

  if (reusableTab) {
    updateTabToUrl(reusableTab, url);
    return
  }
  openNewTab(chrome, allWindows, url);



  /**
 * Find a reusable tab based on the criteria.
 * @param {Array<any>} windows - All Chrome windows.
 * @param {(url: string) => boolean} isUrlReuseable - Function to check if a tab is reusable.
 * @returns {{ tab: object, window: object, tabIndex: number } | null} - Reusable tab or null if not found.
 */
  function findReusableTab(windows, isUrlReuseable) {
    return (
      windows
        .flatMap((window) =>
          window.tabs().map((tab, index) => ({
            tab,
            window,
            tabIndex: index + 1, // 1-based index for Chrome
          }))
        )
        .find(({ tab }) => isUrlReuseable(tab.url())) || null
    );
  }


  /**
  * Update the reusable tab with the new URL.
  * @param {{ tab: object, window: object, tabIndex: number }} reusableTab - The reusable tab details.
  * @param {string} url - The URL to set.
  */
  function updateTabToUrl({ tab, window, tabIndex }, url) {
    if (tab.url() !== url) {
      tab.url = url;
    }
    window.activeTabIndex = tabIndex;
  }

  /**
  * Open a new tab with the specified URL, creating a new window if necessary.
  * @param {object} chrome - The Chrome application instance.
  * @param {Array<any>} allWindows - All Chrome windows.
  * @param {string} url - The URL to open.
  */
  function openNewTab(chrome, allWindows, url) {
    const targetWindow = allWindows.length > 0 ? allWindows[0] : createNewWindow(chrome);
    const newTab = chrome.Tab();
    newTab.url = url;
    targetWindow.tabs.push(newTab);
    targetWindow.activeTabIndex = targetWindow.tabs.length; // Focus the new tab
  }

  /**
  * Create a new Chrome window.
  * @param {object} chrome - The Chrome application instance.
  * @returns {object} - The new Chrome window.
  */
  function createNewWindow(chrome) {
    const newWindow = chrome.Window({}); // Create a new window
    chrome.windows.push(newWindow);
    return newWindow;
  }
}

/**
 * 
 * @param {string} message 
 */
function notifyUserWithAlert(message) {
  const app = Application.currentApplication();
  app.includeStandardAdditions = true;
  app.doShellScript(
    `osascript -e 'display alert "Error" message "${message}" as critical'`
  );
}