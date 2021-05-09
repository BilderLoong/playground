const form = document.getElementsByTagName('form')[0];
const currentModifier = document.getElementById('current-modifier');
const desiredRatio = document.getElementById('desired-ratio');
const currentRatio = document.getElementById('current-ratio');

const result = document.getElementById('result');

form.addEventListener('input', (e) => {
  const inputs = form.getElementsByTagName('input');

  if (Array.from(inputs).every((e) => e.value)) {
    const currentModifierValue = parseFloat(currentModifier.value);
    const desiredRationValue = parseFloat(desiredRatio.value);
    const currentRationValue = parseFloat(currentRatio.value);

    const res = getNewModifier(
      currentModifierValue,
      desiredRationValue,
      currentRationValue
    );

    result.innerHTML = res;
  } else {
    result.innerHTML = 'Please input valid value';
  }
});

function getNewModifier(currentModifier, desiredRatio, currentRatio) {
  return (
    (currentModifier * Math.log10(desiredRatio)) / Math.log10(currentRatio)
  );
}
