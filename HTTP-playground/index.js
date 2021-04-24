(async function () {
  const route = addRoute('1', callBack1);
  addRoute('2', callBack2, route);
  console.log(2);
  console.log(2);

  window.addEventListener('load', () => refresh(route));
  window.addEventListener('hashchange', () => refresh(route));

  // add callback to the current path
  // if no callback or path provided return false
  // else return the route map
  // path don't include #
  function addRoute(path, callback, route = {}) {
    if (!route || !path || !callback) return false;
    route[path] = callback;
    return route;
  }

  // accept a route table,
  // and execute the callback function according
  // to the path
  function refresh(route) {
    const curPath = location.hash.slice(1);
    route[curPath](curPath);
  }

  function callBack1(url) {
    console.log(`callBack1 is called on ${url}`);
  }

  function callBack2(url) {
    console.log(`callBack2 is called on ${url}`);
  }
})();
