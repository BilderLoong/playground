const fs = require("node:fs");
const path = require("node:path");
const Handlebars = require("handlebars");
const Ajv = require("ajv");

// ponytail: attribute-free inline HTML only; use a maintained sanitizer for rich HTML.
// Escape everything first; restore only complete, permitted tags.
const inlineHtml = (value) => new Handlebars.SafeString(
  Handlebars.escapeExpression(value)
    .replace(/&lt;(\/?(?:em|strong|b|i|sub|sup|mark|span)|br\s*\/?)&gt;/gi, "<$1>")
    .replace(/&amp;((?:#\d+|#x[\da-f]+|amp|lt|gt|quot|apos|nbsp));/gi, "&$1;")
);

const synchronizeSchema = (prompt, schema) => prompt.replace(
  /```json\n[\s\S]*?\n```/,
  () => `\`\`\`json\n${JSON.stringify(schema, null, 2)}\n\`\`\``
);

const renderDocument = (prompt, schema, entries) => {
  const validate = new Ajv({ allErrors: true }).compile(schema);
  const invalid = entries.flatMap((entry, index) => validate(entry)
    ? []
    : [{ index, errors: JSON.stringify(validate.errors) }]);
  if (invalid.length) return { ok: false, error: JSON.stringify(invalid) };

  const html = prompt.match(/```html\n([\s\S]*?)\n```/)?.[1];
  if (!html) return { ok: false, error: "The prompt has no HTML template." };
  const templates = [...html.matchAll(/<script id="([^"]+)" type="text\/x-handlebars-template">([\s\S]*?)<\/script>/g)];
  const main = templates.find(([, id]) => id === "main-template");
  if (!main) return { ok: false, error: "The prompt has no main-template." };

  const handlebars = Handlebars.create();
  handlebars.registerHelper("inlineHtml", inlineHtml);
  handlebars.registerHelper("rankLabel", (rank) => ({
    甲: "Prioritize learning", 乙: "Learn when relevant", 丙: "Low priority"
  })[rank] || "");
  templates.filter(([, id]) => id !== "main-template")
    .forEach(([, id, source]) => handlebars.registerPartial(id, source));
  const body = handlebars.compile(main[2])({ entries });
  return {
    ok: true,
    html: html.replace(/<body>[\s\S]*<\/body>/, () => `<body>\n${body}\n</body>`).replace(/[\t ]+$/gm, "")
  };
};

if (require.main === module) {
  try {
    const schema = JSON.parse(fs.readFileSync(path.join(__dirname, "data_schema.json"), "utf8"));
    const promptPath = path.join(__dirname, "etym-schema-prompt.md");
    const prompt = synchronizeSchema(fs.readFileSync(promptPath, "utf8"), schema);
    const entries = ["glutamine", "impetueux"].map((name) => JSON.parse(
      fs.readFileSync(path.join(__dirname, "examples", `${name}.json`), "utf8")
    ));
    const result = renderDocument(prompt, schema, entries);
    if (result.ok) {
      fs.writeFileSync(promptPath, prompt);
      fs.writeFileSync(path.join(__dirname, "test.html"), result.html);
      console.log("Synchronized the prompt schema and rendered glutamine and impétueux to test.html.");
    } else {
      console.error(result.error);
      process.exitCode = 1;
    }
  } catch (error) {
    console.error(error.message);
    process.exitCode = 1;
  }
}

module.exports = { renderDocument, synchronizeSchema };
