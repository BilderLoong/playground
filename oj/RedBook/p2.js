// let line =
//   '{ "value": 3,"left": { "value": 9, "left": {"value": 15, "left": null, "right": null }, "right": { "value": 7, "left": null,"right": null } }, "right": { "value": 9, "left": {"value": 15, "left": null, "right": null }, "right": { "value": 7, "left": null,"right": null } }}';
let line;

while ((line = read_line()) != '') {
  // Your code
  const tree = JSON.parse(line);

  const countNodes = (node) => {
    let res = 0;

    if (!node) return res;

    const traverse = (node) => {
      if (!node.left && !node.right) {
        res++;
      }

      if (node.left) {
        traverse(node.left);
      }

      if (node.right) {
        traverse(node.right);
      }
    };

    traverse(node);
    return res;
  };

  console.log(countNodes(tree));
}
