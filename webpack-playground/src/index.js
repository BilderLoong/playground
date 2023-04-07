import { c, foo } from "./module1";

function component() {
  const element = document.createElement("div");

  // Lodash, now imported by this script
  element.innerHTML = _.join(["Hello", "webpack"], " ");
  console.log(c);
  foo();

  return element;
}

document.body.appendChild(component());
