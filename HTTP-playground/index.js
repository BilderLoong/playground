(async function () {
  try {
    const result = await fetch('http://localhost:81');
    console.log(result);
  } catch (err) {
    console.error(err);
  }
})();
