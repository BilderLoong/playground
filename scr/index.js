// const { jsPDF } = window.jspdf;

const doc =  new jsPDF();
console.log(jsPDF);

doc
  .html(document.body, {
    callback: function (doc) {
      doc.save();
      console.log(1);
    },
    x: 10,
    y: 10,
  })
  .catch((e) => console.error(e));
