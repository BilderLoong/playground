(async function () {
  const foo = Promise.resolve(1);
  const bar = Promise.all([]);
  const baz = Promise.all([1, 2, 3]);
  console.log({ foo });
  console.log({ bar });
  console.log({ baz });

  console.log("foo", await foo);
  console.log("bar", await bar);
  console.log("baz", await baz);

  //   foo.then((val) => {
  //     console.log("foo", val);
  //   });
  //   bar.then((val) => {
  //     console.log("bar", val);
  //   });
  //   baz.then((val) => {
  //     console.log("baz", val);
  //   });
  console.log("didi");
})();
