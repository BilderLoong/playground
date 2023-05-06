(async function () {
  function asyncTimeout(delay) {
    return new Promise((resolve) => {
      setTimeout(() => resolve(delay), delay);
    }).then((d) => `Waited ${d} seconds`);
  }

  function asyncFetch(url) {
    return fetch(url)
      .then((response) => response.text())
      .then((text) => `Fetched ${url}, and got back ${text}`);
  }
  const asyncThingsToDo = [
    { task: "wait", duration: 1000 },
    { task: "fetch", url: "https://httpstat.us/200" },
    { task: "wait", duration: 2000 },
    { task: "fetch", url: "https://httpstat.us/300" },
  ];
  function runTask(spec) {
    return spec.task === "wait"
      ? asyncTimeout(spec.duration)
      : asyncFetch(spec.url);
  }

  // const tasks = asyncThingsToDo.map(runTask); // Run all our tasks in parallel.
  // const results = await Promise.all(tasks); // Gather up the results.
  // results.forEach((x) => console.log(x)); // Print them out on the console.

  const starterPromise = Promise.resolve(null);
  const log = (result) => console.log(result);
  await asyncThingsToDo.reduce(
    (p, spec) => p.then(() => runTask(spec).then(log)),
    starterPromise
  );

  for (const task of asyncThingsToDo) {
    await runTask(task).then((result) => console.log(result));
  }
})();
