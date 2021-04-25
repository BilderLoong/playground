(async function () {
const container = document.querySelector('.container');
const child = document.querySelector('.container > div');

child.addEventListener('click', () => console.log('child got event'));

container.addEventListener(
  'click',
  () => console.log('container got event during bubble'),
);

container.addEventListener(
  'click',
  () => console.log('container got event during capture'),
  true
);
})();
