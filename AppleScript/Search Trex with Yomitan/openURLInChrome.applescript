function run(input, parameters) {
  const app = Application.currentApplication();
  app.includeStandardAdditions = true;

  app.doShellScript(`screencapture -c`);

  const CHROME_NAME = "Google Chrome";
  if (isApplicationRunning(CHROME_NAME)) { return "running" }
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