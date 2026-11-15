# Etymology Tree

## Step 1: Generate JSON data

- Generate a nested JSON data according to the below JSON schema, which should represent the etymological tree of word "{clipboard}".
- No need to reply this JSON data.
- If you can browse the web please find the information on the **[Etymonline] (www.etymonline.com)**, [Wiktionary](https://en.wiktionary.org/wiki/word), and other etymology dictionaries.

### JSON schema

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "title": "Etymology Tree (Node-Based)",
  "description": "Selected etymological paths that help explain a word and connect it to other words.",
  "oneOf": [
    {
      "$ref": "#/definitions/baseNode"
    }
  ],
  "definitions": {
    "usefulness": {
      "type": "string",
      "enum": [
        "甲",
        "乙",
        "丙"
      ],
      "description": "Usefulness for learning or remembering: 甲 = prioritize learning, 乙 = learn when relevant, 丙 = low priority. Consider general usage and usefulness for understanding other English or French words. No frequency lookup is required."
    },
    "sourceUrl": {
      "type": "string",
      "format": "uri",
      "pattern": "^https?://",
      "description": "A source that supports the etymology or shared-root connection."
    },
    "relatedWord": {
      "type": "object",
      "required": [
        "entry",
        "definition",
        "origin_language",
        "connection",
        "source_url",
        "usefulness"
      ],
      "properties": {
        "entry": {
          "$ref": "#/definitions/baseNode/properties/entry"
        },
        "definition": {
          "$ref": "#/definitions/baseNode/properties/definition"
        },
        "origin_language": {
          "$ref": "#/definitions/baseNode/properties/origin_language"
        },
        "connection": {
          "type": "string",
          "description": "Explain the shared root or morpheme and its connection to the current node. Use plain text."
        },
        "source_url": {
          "$ref": "#/definitions/sourceUrl"
        },
        "usefulness": {
          "$ref": "#/definitions/usefulness"
        }
      },
      "additionalProperties": false
    },
    "atomicTypes": {
      "type": "string",
      "enum": [
        "root",
        "prefix",
        "suffix",
        "infix",
        "connector"
      ],
      "description": "Elements that cannot be broken down further."
    },
    "breakableTypes": {
      "type": "string",
      "enum": [
        "compound",
        "phrase"
      ],
      "description": "Elements that are built from smaller components."
    },
    "baseNode": {
      "type": "object",
      "required": [
        "entry",
        "definition"
      ],
      "properties": {
        "type": {
          "type": "string",
          "enum": [
            "root",
            "prefix",
            "suffix",
            "infix",
            "connector",
            "compound",
            "phrase"
          ]
        },
        "entry": {
          "type": "string",
          "contentMediaType": "text/html",
          "description": "The word or morpheme being analyzed, as an inline HTML fragment. Use plain text or attribute-free em, strong, b, i, sub, sup, mark, span, and br tags."
        },
        "definition": {
          "type": "string",
          "contentMediaType": "text/html",
          "description": "The concise meaning of this entry, as an inline HTML fragment. Use plain text or attribute-free em, strong, b, i, sub, sup, mark, span, and br tags."
        },
        "usefulness": {
          "$ref": "#/definitions/usefulness"
        },
        "source_url": {
          "$ref": "#/definitions/sourceUrl"
        },
        "origin_language": {
          "type": "object",
          "required": [
            "iso_639_3_code",
            "full_language_name"
          ],
          "properties": {
            "iso_639_3_code": {
              "type": "string"
            },
            "full_language_name": {
              "type": "string"
            }
          }
        },
        "notes": {
          "type": "string",
          "description": "Additional information or commentary about this entry. Like etymological nuances, historical context, or interesting trivia and how meaning has shifted over time."
        },
        "related_words": {
          "type": "array",
          "description": "A selected list of English and French words that share a root or morpheme with this node. Explain each connection. Include both languages when supported by evidence; use an empty list when no reliable examples are available.",
          "items": {
            "$ref": "#/definitions/relatedWord"
          }
        },
        "children": {
          "type": "array",
          "description": "Curated recursive relationships representing the most pedagogically useful etymological path for understanding this entry. Children may represent structural components, historical ancestors, borrowings, or other significant etymological relations. This field is intentionally selective rather than exhaustive. Choose to show those most help me to understand the word's meaning and could help me to learn other words in the future.",
          "items": {
            "$ref": "#"
          }
        }
      }
    }
  }
}
```

## Step 2: Render the trees

- Render the trees with the template below using `{ entries: [tree, ...] }`. Register the partials, `inlineHtml` for safe schema-permitted markup, and `rankLabel` for the rank meanings in the schema.
- Please provide only the final, rendered HTML text that would appear inside the `<body>` tag.

```html
<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>Etymology trees</title>
  <style>
    :root { color-scheme: light; font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif; color: #252a2d; background: #fafaf8; -webkit-font-smoothing: antialiased; }
    * { box-sizing: border-box; }
    body { max-width: 1020px; margin: 0 auto; padding: 24px 28px 40px; font-size: 14px; line-height: 1.45; }
    h1 { margin: 0; font-size: 24px; letter-spacing: -0.035em; }
    .intro { margin: 4px 0 8px; color: #626967; }
    .legend { display: flex; flex-wrap: wrap; gap: 6px 16px; margin-bottom: 16px; font-size: 12px; color: #626967; }
    .legend span { display: inline-flex; align-items: center; gap: 6px; }
    .entry-card { padding: 18px 20px; margin-top: 16px; border: 1px solid #dedfd9; border-radius: 10px; background: white; }
    .tree, .children, .related-list { list-style: none; padding: 0; margin: 0; }
    .entry-heading { display: flex; align-items: baseline; gap: 7px; flex-wrap: wrap; }
    .entry-word { font-size: 16px; font-weight: 650; letter-spacing: -0.015em; }
    h2.entry-word { margin: 0; font-size: 26px; letter-spacing: -0.035em; }
    .entry-meta, .entry-type, .usefulness { display: inline-block; padding: 0 6px; border-radius: 4px; font-size: 12px; line-height: 20px; font-weight: 650; background: #f0f1ef; color: #5e6661; }
    .entry-meta, .entry-type { font-weight: 500; }
    .usefulness[data-rank="甲"] { background: #e7f0e9; color: #315d42; }
    .usefulness[data-rank="乙"] { background: #f5eddc; color: #795c26; }
    .definition { margin: 2px 0 4px; text-wrap: pretty; }
    .notes { margin: 0 0 4px; color: #626967; font-size: 13px; text-wrap: pretty; }
    .source { color: #46665c; font-size: 12px; text-underline-offset: 3px; }
    a:hover { color: #1a4534; }
    a:focus-visible, summary:focus-visible { outline: 2px solid #46665c; outline-offset: 4px; border-radius: 2px; }
    .children { border-left: 1px solid #d9dfda; margin: 10px 0 0 6px; padding-left: 18px; }
    .children > .entry + .entry { margin-top: 12px; }
    .related { margin-top: 2px; }
    .related summary { width: fit-content; cursor: pointer; color: #46665c; font-size: 12px; padding: 4px 0; min-height: 40px; }
    .related-list { display: grid; grid-template-columns: repeat(2, minmax(0, 1fr)); gap: 8px 16px; padding: 4px 0; }
    .related-word { padding-left: 10px; border-left: 2px solid #e4e8e2; }
    .related-word .entry-word { font-size: 14px; }
    .related-word .definition { font-size: 13px; margin-bottom: 3px; }
    .connection { margin: 0; color: #69716d; font-size: 12px; }
    mark { background: #f5eddc; color: inherit; padding: 0 2px; }
    @media (max-width: 640px) { body { padding: 18px 12px 28px; } .entry-card { padding: 14px 12px; } .children { padding-left: 12px; } .related-list { grid-template-columns: 1fr; } }
  </style>
</head>
<body>
  <script id="main-template" type="text/x-handlebars-template">
    <main>
      {{#each entries}}
        <article class="entry-card" id="entry-{{@index}}">
          <ul class="tree">{{> entryPartial this isRoot=true}}</ul>
        </article>
      {{/each}}
    </main>
  </script>
  <script id="entryPartial" type="text/x-handlebars-template">
    <li class="entry">
      <div class="entry-heading">
        {{#if isRoot}}<h2 class="entry-word">{{inlineHtml entry}}</h2>{{else}}<span class="entry-word">{{inlineHtml entry}}</span>{{/if}}
        {{#if origin_language}}<span class="entry-meta" title="{{origin_language.full_language_name}}">{{origin_language.iso_639_3_code}}</span>{{/if}}
        {{#if type}}<span class="entry-type">{{type}}</span>{{/if}}
        {{#if usefulness}}<span class="usefulness" data-rank="{{usefulness}}" title="{{rankLabel usefulness}}" aria-label="{{usefulness}}: {{rankLabel usefulness}}">{{usefulness}}</span>{{/if}}
        {{#if source_url}}<a class="source" href="{{source_url}}" aria-label="Etymology source">Source</a>{{/if}}
      </div>
      <p class="definition">{{inlineHtml definition}}</p>
      {{#if notes}}<p class="notes">{{notes}}</p>{{/if}}
      {{#if related_words}}
        <details class="related" {{#if isRoot}}open{{/if}}>
          <summary>Related words · {{related_words.length}}</summary>
          <ul class="related-list">{{#each related_words}}{{> relatedWordPartial this}}{{/each}}</ul>
        </details>
      {{/if}}
      {{#if children}}
        <ul class="children">{{#each children}}{{> entryPartial this isRoot=false}}{{/each}}</ul>
      {{/if}}
    </li>
  </script>
  <script id="relatedWordPartial" type="text/x-handlebars-template">
    <li class="related-word">
      <div class="entry-heading">
        <span class="entry-word">{{inlineHtml entry}}</span>
        <span class="entry-meta" title="{{origin_language.full_language_name}}">{{origin_language.iso_639_3_code}}</span>
        <span class="usefulness" data-rank="{{usefulness}}" title="{{rankLabel usefulness}}" aria-label="{{usefulness}}: {{rankLabel usefulness}}">{{usefulness}}</span>
      </div>
      <p class="definition">{{inlineHtml definition}}</p>
      <p class="connection">{{connection}} <a class="source" href="{{source_url}}">Source</a></p>
    </li>
  </script>
</body>
</html>
```
