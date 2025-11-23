# Etymology Tree

## Step 1: Generate JSON data
Generate a nested JSON data according to the below JSON schema, which should represent the etymological tree of word "insertion".
No need to reply this JSON data.

### Info

If you can browse the web please find the information on the **[Etymonline] (www.etymonline.com)**, [Wiktionary](https://en.wiktionary.org/wiki/word), and other etymology dictionaries. Otherwise, recall HARDLY the information of the **[Etymonline](www.etymonline.com)**, Wiktionary, and other etymology dictionaries from your memory.

### JSON schema

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "title": "Etymology Tree",
  "description": "A schema for representing etymological word breakdowns in a hierarchical structure",
  "definitions": {
    "etymologyEntryTypes": {
      "type": "string",
      "oneOf": [
        {
          "$ref": "#/definitions/etymologyEntryBreakableTypes"
        },
        {
          "$ref": "#/definitions/etymologyEntryUnbreakableTypes"
        }
      ],
      "description": "The type of etymological entry."
    },
    "etymologyEntryBreakableTypes": {
      "type": "string",
      "oneOf": [
        {
          "const": "compound",
          "description": "A word formed by combining two or more independent words or morphemes",
          "example": "firefighter (fire + fighter)"
        },
        {
          "const": "phrase",
          "description": "A group of words functioning as a unit in etymology",
          "example": "sine die (without a day specified)"
        }
      ],
      "description": "The type of etymological entry that can be further breakdown."
    },
    "etymologyEntryUnbreakableTypes": {
      "type": "string",
      "oneOf": [
        {
          "const": "root",
          "description": "A base morpheme that carries the core meaning and cannot be further divided",
          "example": "bio (meaning \"life\") in biology"
        },
        {
          "const": "prefix",
          "description": "A morpheme added to the beginning of a word to modify its meaning",
          "example": "un- (meaning \"not\") in unhappy"
        },
        {
          "const": "suffix",
          "description": "A morpheme added to the end of a word to modify its meaning or change its grammatical function",
          "example": "-tion (forms a noun) in creation"
        },
        {
          "const": "infix",
          "description": "A morpheme inserted within a word to modify its meaning",
          "example": "-um- in Tagalog b-um-ili (bought)"
        },
        {
          "const": "connector",
          "description": "A linking element between morphemes or words in compounds",
          "example": "o in therm-o-meter"
        }
      ],
      "description": "The type of etymological entry that cannot be further broken down."
    },
    "baseEtymologyEntry": {
      "type": "object",
      "required": [
        "entry",
        "type",
        "definition"
      ],
      "properties": {
        "entry": {
          "type": "string",
          "description": "The word or morpheme being analyzed",
          "examples": [
            "preponderance",
            "hurry up",
            "pre-",
            "-tion"
          ]
        },
        "type": {
          "$ref": "#/definitions/etymologyEntryTypes"
        },
        "origin_language": {
          "type": "object",
          "description": "The language of the entry.",
          "required": [
            "iso_639_3_code",
            "full_language_name"
          ],
          "properties": {
            "iso_639_3_code": {
              "type": "string",
              "description": "Language using ISO 639-3 code"
            },
            "full_language_name": {
              "type": "string",
              "description": "Full language name of the ISO 639-3 code"
            }
          }
        },
        "definition": {
          "type": "string",
          "description": "The concise meaning of this entry."
        },
        "notes": {
          "type": "string",
          "description": "Additional etymological notes or historical/cultural context."
        },
        "examples": {
          "type": "array",
          "items": {
            "type": "object",
            "required": [
              "word",
              "simple_definition",
              "language"
            ],
            "properties": {
              "word": {
                "type": "string",
                "description": "Example word using this current entry from the root entry language."
              },
              "simple_definition": {
                "type": "string",
                "description": "Simple definition of the example word."
              },
              "language": {
                "type": "string",
                "description": "Full Language name of the example word."
              }
            }
          }
        }
      }
    }
  },
  "allOf": [
    {
      "$ref": "#/definitions/baseEtymologyEntry"
    },
    {
      "$comment": "Rule: Components are REQUIRED for compound and phrase types at root level",
      "if": {
        "properties": {
          "type": {
            "$ref": "#/definitions/etymologyEntryBreakableTypes"
          }
        },
        "required": [
          "type"
        ]
      },
      "then": {
        "required": [
          "components"
        ]
      }
    }
  ],
  "properties": {
    "ancestor": {
      "$ref": "#/definitions/baseEtymologyEntry",
      "description": "Two type of tracing. 1. Historical Tracing: doing diachronic analysis to find the further one level trace back of the entry to its ancestor root word or morpheme. If possible, you should trace to the further most ancestor root word or morpheme till you can't. Some common most deep level: the PIE for Indo-European languages, Proto-Afro-Asiatic for Afro-Asiatic, Proto-Sino-Tibetan for Sino-Tibetan, Proto-Austronesian for Austronesian, Proto-Dravidian for Dravidian, Proto-Uralic for Uralic, Proto-Bantu for Niger-Congo (Bantu branch) and etc." 2. part-of-speech/conversion tracing.,
      "examples": [
        "The word 'dictionary' is come from Medieval Latin word: 'dictionarium', which is derived from Latin 'dictio', which in turn comes from the root 'dicere' ."
      ]
    },
    "components": {
      "type": "array",
      "description": "Breakdown of this entry into its morpheme parts, only allow for etymologyEntryBreakableTypes, you should find proper levels to breakdown, no need to break down at each level, just find the most appropriate level so that is easier to understand and those breakdown morphemes are easier to apply to other situations.",
      "items": {
        "$ref": "#/definitions/baseEtymologyEntry"
      }
    }
  }
}
```


## Step 2: Render the generate JSON data to HTML in a nested list by using handlebar template

Using the provided Handlebars template and JSON data, render the final HTML output. The main template is main-template and it uses the recursive partial entryPartial. Please provide only the final, rendered HTML content that would appear inside the `<body>` tag.

```html
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <title>Etymology of {{entry}}</title>
    <style>
        body { font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Helvetica, Arial, sans-serif; line-height: 1.6; color: #333; max-width: 800px; margin: 20px auto; padding: 0 15px; }
        .etymology-tree { border-left: 2px solid #e0e0e0; padding-left: 20px; }
        .entry { margin-top: 15px; }
        .entry-header { margin-bottom: 5px; }
        .entry-word { font-weight: bold; font-size: 1.2em; color: #1a1a1a; }
        .entry-type { font-style: italic; color: #666; }
        .entry-lang { font-style: italic; color: #005a9c; }
        .definition { margin-top: 0; margin-bottom: 10px; }
        .section { margin-top: 10px; padding-left: 20px; border-left: 2px solid #f0f0f0; }
        .section-title { font-weight: bold; margin-bottom: 5px; }
        ul { list-style-type: '→'; padding-left: 20px; }
    </style>
</head>
<body>

    <script id="main-template" type="text/x-handlebars-template">
        <h1>Etymology of "{{entry}}"</h1>
        <div class="etymology-tree">
            {{> entryPartial .}}
        </div>
    </script>

    <script id="entryPartial" type="text/x-handlebars-template">
        <div class="entry">
            <div class="entry-header">
                <span class="entry-word">{{entry}}</span>
                <span class="entry-type">({{type}})</span>
                {{#if origin_language}}
                    <span class="entry-lang">[{{origin_language.full_language_name}}]</span>
                {{/if}}
            </div>
            <p class="definition">{{definition}}</p>

            {{#if notes}}
                <div class="section">
                    <div class="section-title">Notes:</div>
                    <p>{{notes}}</p>
                </div>
            {{/if}}
            
            {{#if ancestor}}
                <div class="section">
                    <div class="section-title">Ancestor:</div>
                    {{! Recursive call for the ancestor }}
                    {{> entryPartial ancestor}}
                </div>
            {{/if}}

            {{#if components}}
                <div class="section">
                    <div class="section-title">Components:</div>
                    <ul>
                        {{#each components}}
                            <li>
                                {{! Recursive call for each component }}
                                {{> entryPartial this}}
                            </li>
                        {{/each}}
                    </ul>
                </div>
            {{/if}}

            {{#if examples}}
                <div class="section">
                    <div class="section-title">Examples:</div>
                    <ul>
                        {{#each examples}}
                            <li><strong>{{word}}</strong> ({{language}}): {{simple_definition}}</li>
                        {{/each}}
                    </ul>
                </div>
            {{/if}}
        </div>
    </script>

</body>
</html>
```