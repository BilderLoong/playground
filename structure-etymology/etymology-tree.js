/**
 * @typedef {Object} Language
 * @property {string} iso_639_3_code - ISO 639-3 language code
 * @property {string} full_language_name - Full name of the language
 */

/**
 * @typedef {Object} Example
 * @property {string} word - Example word
 * @property {string} simple_definition - Simple definition of the example
 * @property {string} language - Language of the example
 */

/**
 * @typedef {Object} EtymologyComponent
 * @property {string} type - Type of component (root, prefix, suffix, etc.)
 * @property {string} component - The component text
 * @property {string} definition - Definition of the component
 */

/**
 * @typedef {Object} EtymologyEntry
 * @property {string} entry - The word or morpheme
 * @property {string} type - Type of entry
 * @property {string} definition - Definition of the entry
 * @property {Language} [origin_language] - Language origin information
 * @property {string} [notes] - Additional notes
 * @property {Example[]} [examples] - Example usages
 * @property {EtymologyComponent[]} [components] - Component breakdown
 * @property {EtymologyEntry[]} [children] - Child entries
 */

/**
 * Creates a semantic HTML structure for an etymology tree
 * @param {EtymologyEntry} entry - The etymology entry to render
 * @param {number} [level=0] - Current nesting level
 * @returns {HTMLElement} The rendered HTML element
 */
export function renderEtymologyTree(entry, level = 0) {
  const details = document.createElement('details');
  details.className = `etymology-entry etymology-level-${level}`;
  details.dataset.type = entry.type;
  details.setAttribute('open', '');

  // Create summary section
  const summary = document.createElement('summary');
  summary.className = 'etymology-header';
  
  const word = document.createElement('h2');
  word.className = 'etymology-entry';
  word.textContent = entry.entry;
  
  const type = document.createElement('span');
  type.className = 'etymology-type';
  type.textContent = entry.type;
  
  // Add description based on type
  const typeDescriptions = {
    'root': 'A base morpheme that carries the core meaning and cannot be further divided',
    'prefix': 'A morpheme added to the beginning of a word to modify its meaning',
    'suffix': 'A morpheme added to the end of a word to modify its meaning or change its grammatical function',
    'infix': 'A morpheme inserted within a word to modify its meaning',
    'connector': 'A linking element between morphemes or words in compounds',
    'compound': 'A word formed by combining two or more independent words or morphemes',
    'phrase': 'A group of words functioning as a unit in etymology'
  };
  
  if (typeDescriptions[entry.type]) {
    type.title = typeDescriptions[entry.type];
  }
  
  summary.appendChild(word);
  summary.appendChild(type);

  // Add language tag if present
  if (entry.origin_language) {
    const language = document.createElement('span');
    language.className = 'etymology-language';
    const abbr = document.createElement('abbr');
    abbr.textContent = entry.origin_language.iso_639_3_code;
    abbr.title = entry.origin_language.full_language_name;
    language.appendChild(abbr);
    summary.appendChild(language);
  }

  details.appendChild(summary);

  // Create content section
  const content = document.createElement('div');
  content.className = 'etymology-content';

  // Add definition
  const definition = document.createElement('p');
  definition.className = 'etymology-definition';
  definition.textContent = entry.definition;
  content.appendChild(definition);

  // Add notes if present
  if (entry.notes) {
    const notes = document.createElement('p');
    notes.className = 'etymology-notes';
    notes.textContent = entry.notes;
    content.appendChild(notes);
  }

  // Add examples if present
  if (entry.examples?.length) {
    const examples = document.createElement('div');
    examples.className = 'etymology-examples';
    entry.examples.forEach(example => {
      const exampleEl = document.createElement('div');
      exampleEl.className = 'etymology-example';
      exampleEl.innerHTML = `
        <strong>${example.word}</strong>
        <span>${example.simple_definition}</span>
        <small>${example.language}</small>
      `;
      examples.appendChild(exampleEl);
    });
    content.appendChild(examples);
  }

  // Add components if present
  if (entry.components?.length) {
    const components = document.createElement('div');
    components.className = 'etymology-components';
    entry.components.forEach(component => {
      const componentEl = document.createElement('div');
      componentEl.className = 'etymology-component';
      componentEl.innerHTML = `
        <span class="component-type">${component.type}</span>
        <span class="component-text">${component.component}</span>
        <span class="component-definition">${component.definition}</span>
      `;
      components.appendChild(componentEl);
    });
    content.appendChild(components);
  }

  details.appendChild(content);

  // Add children if present
  if (entry.children?.length) {
    const children = document.createElement('div');
    children.className = 'etymology-children';
    entry.children.forEach(child => {
      children.appendChild(renderEtymologyTree(child, level + 1));
    });
    details.appendChild(children);
  }

  return details;
} 