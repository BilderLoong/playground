const assert = require("node:assert/strict");
const fs = require("node:fs");
const path = require("node:path");
const test = require("node:test");
const Ajv = require("ajv");
const { JSDOM } = require("jsdom");
const { renderDocument, synchronizeSchema } = require("./render.cjs");

test("the canonical schema, prompt, and safe recursive render agree", () => {
  const schema = JSON.parse(fs.readFileSync(path.join(__dirname, "data_schema.json"), "utf8"));
  const prompt = fs.readFileSync(path.join(__dirname, "etym-schema-prompt.md"), "utf8");
  const embedded = JSON.parse(prompt.match(/```json\n([\s\S]*?)\n```/)[1]);
  assert.deepEqual(embedded, schema);
  assert.equal(synchronizeSchema(prompt, schema), prompt);
  assert.equal(prompt.includes("AI-"), false);
  assert.equal(schema.definitions.baseNode.properties.entry.contentMediaType, "text/html");
  assert.equal(schema.definitions.baseNode.properties.definition.contentMediaType, "text/html");

  const entries = ["glutamine", "impetueux"].map((name) => JSON.parse(
    fs.readFileSync(path.join(__dirname, "examples", `${name}.json`), "utf8")
  ));
  const original = JSON.stringify(entries);
  const validate = new Ajv({ allErrors: true }).compile(schema);
  entries.forEach((entry) => assert.equal(validate(entry), true, JSON.stringify(validate.errors)));
  const checkNodes = (node) => {
    assert.ok(["甲", "乙", "丙"].includes(node.usefulness));
    assert.ok(Array.isArray(node.related_words));
    assert.deepEqual(new Set(node.related_words.map((word) => word.origin_language.iso_639_3_code)), new Set(["eng", "fra"]));
    node.related_words.forEach((word) => assert.ok(["甲", "乙", "丙"].includes(word.usefulness)));
    (node.children || []).forEach(checkNodes);
  };
  entries.forEach(checkNodes);
  assert.equal(validate({ ...entries[0], usefulness: "丁" }), false);
  assert.equal(validate({ ...entries[0], children: [{ entry: "root", definition: "root", usefulness: "丁" }] }), false);
  assert.equal(validate({ ...entries[0], related_words: [{ ...entries[0].related_words[0], usefulness: "丁" }] }), false);
  assert.equal(validate({ ...entries[0], source_url: "javascript:alert(1)" }), false);
  assert.equal(validate({ ...entries[0], entry: 42 }), false);

  const result = renderDocument(prompt, schema, entries);
  assert.equal(result.ok, true, result.error);
  assert.equal(/[\t ]+$/m.test(result.html), false);
  const document = new JSDOM(result.html).window.document;
  assert.deepEqual([...document.querySelectorAll("h2")].map((node) => node.textContent), ["glutamine", "impétueux"]);
  assert.equal(document.querySelectorAll("article").length, 2);
  assert.ok(document.querySelector(".children .children .children"));
  assert.ok(document.querySelector(".definition sub"));
  assert.ok(document.querySelector(".entry-word em"));
  assert.ok(document.querySelector(".related-word strong"));
  assert.match(document.querySelector("article").textContent, /eng[\s\S]*fra/);
  assert.equal(document.querySelectorAll("script").length, 0);
  assert.equal(result.html.includes("{{"), false);
  assert.equal(JSON.stringify(entries), original);

  const hostile = [{ ...entries[0],
    entry: '<strong onclick="alert(1)">unsafe</strong><em>safe</em><img src=x onerror=alert(1)>',
    definition: '<script>alert(1)</script><svg onload=alert(1)></svg><strong>safe</strong> &amp; &lt;img&gt; &#60;img&#62;'
  }];
  const safe = renderDocument(prompt, schema, hostile);
  assert.equal(safe.ok, true, safe.error);
  const safeDocument = new JSDOM(safe.html).window.document;
  assert.equal(safeDocument.querySelector("script, img, svg, [onclick], [onerror], [onload]"), null);
  assert.equal(safeDocument.querySelector("h2 em").textContent, "safe");
  assert.ok(safeDocument.querySelector(".definition strong"));
  assert.match(safeDocument.querySelector(".definition").textContent, /& <img> <img>/);
  assert.equal(renderDocument(prompt, schema, [{ entry: "missing definition" }]).ok, false);
  assert.equal(renderDocument("missing template", schema, entries).ok, false);
});
