

/**
 * @param {string} input
 * @param {string=} parameters
 */
function run(input, parameters) {
  const app = Application.currentApplication();
  app.includeStandardAdditions = true;
  try {
    // Read the file content
    // const text = input[0]
    const text = 1
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
  const CHROME_NAME = "Google Chrome"

  // Because the chrome.activate(); default open a chrome instance with new profile, which isn't desire effect.
  // So we need to use shell command to open a new chrome instance.
  // But I don't know why the `--profile-directory="Default"` doesn't work in shortcuts app. Sad.
  // if (!isApplicationRunning(CHROME_NAME)) {
  //   const app = Application.currentApplication();
  //   app.includeStandardAdditions = true;
  //   app.doShellScript(`open -na "Google Chrome" --args --profile-directory="Default" ${url}`);
  //   return;
  // }

  const chrome = Application(CHROME_NAME);
  chrome.activate();
  const allWindows = chrome.windows();
  const reusableTab = findReusableTab(allWindows, isUrlReuseable);
  console.log("reusableTab", reusableTab);
  if (reusableTab) {
    updateTabToUrl(reusableTab, url);
    return
  }
  console.log(JSON.stringify({ allWindows }))
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
    const targetWindow = allWindows.length > 0 ? allWindows[0] : chrome.Window().make();
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

/**
 * https://stackoverflow.com/questions/46628576/how-do-you-run-inline-applescript-from-a-jxa-javascript-for-automation-script-on
 * @param {string} s 
 * @returns 
 */
function evalAS(s) {
  const a = Application.currentApplication();
  return (a.includeStandardAdditions = true, a)
    .runScript(s);
}

// Function to check if an application is running
/**
 * @param {string} appName
 * @returns {boolean}
 */
function isApplicationRunning(appName) {
  const systemEvents = Application("System Events");
  const runningApps = systemEvents.applicationProcesses.where({ name: appName });
  return runningApps.length > 0;
}
