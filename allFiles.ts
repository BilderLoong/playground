const allFiles: string[] = [];

allFiles.forEach((path: string) => () => {
  (async function () {
    const res = await fs.readFile(path);
    console.log(res);
  })();
});