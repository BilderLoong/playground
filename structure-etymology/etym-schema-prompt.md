Generate JSON according to the below JSON schema and example result., which explains the etymology of word "insertion".

## JSON scheam
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
        },
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
      "description": "The type of etymological entry."
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
    "etymologyComponent": {
      "type": "object",
      "required": [
        "type",
        "component",
        "definition"
      ],
      "properties": {
        "type": {
          "$ref": "#/definitions/etymologyEntryUnbreakableTypes"
        },
        "component": {
          "type": "string",
          "description": "The component of the entry."
        },
        "definition": {
          "type": "string",
          "description": "The meaning of the component."
        }
      }
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
          "description": "The word or morpheme being analyzed"
        },
        "type": {
          "$ref": "#/definitions/etymologyEntryTypes"
        },
        "origin_language": {
          "type": "object",
          "description": "Language origin of the entry.",
          "required": [
            "iso_639_3_code",
            "full_language_name"
          ],
          "properties": {
            "iso_639_3_code": {
              "type": "string",
              "description": "Language origin using ISO 639-3 code"
            },
            "full_language_name": {
              "type": "string",
              "description": "Full language name of the ISO 639-3 code"
            }
          }
        },
        "definition": {
          "type": "string",
          "description": "The meaning of this entry."
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
                "description": "Language of the example word."
              }
            }
          }
        }
      }
    },
    "childEtymologyEntry": {
      "description": "Child etymology entry that prohibits components property",
      "allOf": [
        {
          "$ref": "#/definitions/baseEtymologyEntry"
        },
        {
          "properties": {
            "components": false,
            "children": {
              "type": "array",
              "description": "Further etymology tracing of this entry.",
              "items": {
                "$ref": "#/definitions/childEtymologyEntry"
              }
            }
          }
        }
      ]
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
    "components": {
      "type": "array",
      "description": "Simplely directly breakdown of this entry into its morpheme parts. Only allowed at root level for types compound and phrase.",
      "items": {
        "$ref": "#/definitions/etymologyComponent"
      }
    },
    "children": {
      "type": "array",
      "description": "Further etymological tracing to source language till PIE or component breakdown till unbreakable.",
      "items": {
        "$ref": "#/definitions/childEtymologyEntry"
      }
    }
  }
}
```

## Examples

### preponderance
```json
{
    "entry": "preponderance",
    "type": "compound",
    "definition": "The quality or fact of being greater in number, quantity, or importance",
    "origin_language": {
      "iso_639_3_code": "eng",
      "full_language_name": "English"
    },
    "notes": "Entered English in the 17th century from Latin via French influence",
    "examples": [
      {
        "word": "preponderance of evidence",
        "simple_definition": "The greater weight of evidence in a legal case",
        "language": "English"
      },
      {
        "word": "preponderance in numbers",
        "simple_definition": "A majority or greater quantity in count",
        "language": "English"
      }
    ],
    "components": [
      {
        "type": "prefix",
        "component": "pre-",
        "definition": "Before, in front of, or prior to"
      },
      {
        "type": "root",
        "component": "ponder",
        "definition": "To weigh or consider"
      },
      {
        "type": "suffix",
        "component": "-ance",
        "definition": "The action or state of (forming a noun)"
      }
    ],
    "children": [
      {
        "entry": "pre-",
        "type": "prefix",
        "definition": "Before, in front of, or prior to",
        "origin_language": {
          "iso_639_3_code": "lat",
          "full_language_name": "Latin"
        },
        "examples": [
          {
            "word": "predict",
            "simple_definition": "To foretell or say in advance",
            "language": "English"
          },
          {
            "word": "precede",
            "simple_definition": "To go before in time or order",
            "language": "English"
          }
        ],
        "children": [
          {
            "entry": "prae-",
            "type": "prefix",
            "definition": "Before, in front of",
            "origin_language": {
              "iso_639_3_code": "lat",
              "full_language_name": "Latin"
            },
            "notes": "Inherited from Proto-Italic and ultimately Proto-Indo-European",
            "examples": [
              {
                "word": "preamble",
                "simple_definition": "An introductory statement",
                "language": "English"
              },
              {
                "word": "prefix",
                "simple_definition": "Something placed before",
                "language": "English"
              }
            ],
            "children": [
              {
                "entry": "*preh₂i-",
                "type": "root",
                "definition": "Before, in front of",
                "origin_language": {
                  "iso_639_3_code": "ine",
                  "full_language_name": "Proto-Indo-European"
                },
                "notes": "Reconstructed PIE root, source of many 'before' prefixes",
                "examples": [
                  {
                    "word": "prepare",
                    "simple_definition": "To make ready beforehand",
                    "language": "English"
                  },
                  {
                    "word": "prevent",
                    "simple_definition": "To stop something before it happens",
                    "language": "English"
                  }
                ]
              }
            ]
          }
        ]
      },
      {
        "entry": "ponderare",
        "type": "root",
        "definition": "To weigh, to consider",
        "origin_language": {
          "iso_639_3_code": "lat",
          "full_language_name": "Latin"
        },
        "examples": [
          {
            "word": "ponder",
            "simple_definition": "To think deeply or weigh in the mind",
            "language": "English"
          },
          {
            "word": "preponderate",
            "simple_definition": "To exceed in weight or influence",
            "language": "English"
          }
        ],
        "children": [
          {
            "entry": "pondus",
            "type": "root",
            "definition": "Weight",
            "origin_language": {
              "iso_639_3_code": "lat",
              "full_language_name": "Latin"
            },
            "notes": "Derived from an earlier root meaning 'to weigh' or 'hang'",
            "examples": [
              {
                "word": "pound",
                "simple_definition": "A unit of weight",
                "language": "English"
              },
              {
                "word": "pendulum",
                "simple_definition": "A weight hanging to swing",
                "language": "English"
              }
            ],
            "children": [
              {
                "entry": "*pend-",
                "type": "root",
                "definition": "To hang, to weigh",
                "origin_language": {
                  "iso_639_3_code": "ine",
                  "full_language_name": "Proto-Indo-European"
                },
                "notes": "Reconstructed PIE root linked to concepts of weight and suspension",
                "examples": [
                  {
                    "word": "depend",
                    "simple_definition": "To hang down or rely on",
                    "language": "English"
                  },
                  {
                    "word": "pendant",
                    "simple_definition": "An ornament that hangs",
                    "language": "English"
                  }
                ]
              }
            ]
          }
        ]
      },
      {
        "entry": "-ance",
        "type": "suffix",
        "definition": "The action or state of (forming a noun)",
        "origin_language": {
          "iso_639_3_code": "lat",
          "full_language_name": "Latin"
        },
        "examples": [
          {
            "word": "reliance",
            "simple_definition": "The state of depending on something",
            "language": "English"
          },
          {
            "word": "assistance",
            "simple_definition": "The action of helping",
            "language": "English"
          }
        ],
        "children": [
          {
            "entry": "-antia",
            "type": "suffix",
            "definition": "The quality or state of (forming nouns)",
            "origin_language": {
              "iso_639_3_code": "lat",
              "full_language_name": "Latin"
            },
            "notes": "Derived from a participial form, often tied to verbs",
            "examples": [
              {
                "word": "resistance",
                "simple_definition": "The act of opposing",
                "language": "English"
              },
              {
                "word": "distance",
                "simple_definition": "The extent of space between two points",
                "language": "English"
              }
            ],
            "children": [
              {
                "entry": "*-nt-",
                "type": "root",
                "definition": "Participial suffix indicating action or state",
                "origin_language": {
                  "iso_639_3_code": "ine",
                  "full_language_name": "Proto-Indo-European"
                },
                "notes": "Common PIE suffix for forming participles and nouns",
                "examples": [
                  {
                    "word": "abundance",
                    "simple_definition": "A large quantity",
                    "language": "English"
                  },
                  {
                    "word": "vigilance",
                    "simple_definition": "The state of being watchful",
                    "language": "English"
                  }
                ]
              }
            ]
          }
        ]
      }
    ]
  }
```

### insertion
```json
{
  "entry": "insertion",
  "type": "compound",
  "definition": "The action of inserting or placing something into something else.",
  "origin_language": {
    "iso_639_3_code": "eng",
    "full_language_name": "English"
  },
  "notes": "Derived from the verb 'insert' with the noun-forming suffix '-ion'.",
  "examples": [
    {
      "word": "insertion point",
      "simple_definition": "The place where something is inserted.",
      "language": "English"
    },
    {
      "word": "gene insertion",
      "simple_definition": "The process of inserting a gene into a DNA sequence.",
      "language": "English"
    }
  ],
  "components": [
    {
      "type": "root",
      "component": "insert",
      "definition": "To put or place into something; to thrust in."
    },
    {
      "type": "suffix",
      "component": "-ion",
      "definition": "Action or process; state or condition (forming nouns from verbs)."
    }
  ],
  "children": [
    {
      "entry": "insert",
      "type": "root",
      "definition": "To put or place into something; to thrust in.",
      "origin_language": {
        "iso_639_3_code": "eng",
        "full_language_name": "English"
      },
      "examples": [
        {
          "word": "insert coin",
          "simple_definition": "A common instruction on vending machines.",
          "language": "English"
        },
        {
          "word": "insert key",
          "simple_definition": "To place a key into a lock.",
          "language": "English"
        }
      ],
      "children": [
        {
          "entry": "inserere",
          "type": "root",
          "definition": "To put in, introduce, insert.",
          "origin_language": {
            "iso_639_3_code": "lat",
            "full_language_name": "Latin"
          },
          "notes": "Latin verb from which 'insert' is derived.",
          "examples": [
            {
              "word": "inserere digitum",
              "simple_definition": "To insert a finger.",
              "language": "Latin"
            }
          ],
          "children": [
            {
              "entry": "in-",
              "type": "prefix",
              "definition": "In, into, on, upon.",
              "origin_language": {
                "iso_639_3_code": "lat",
                "full_language_name": "Latin"
              },
              "examples": [
                {
                  "word": "include",
                  "simple_definition": "To enclose within; contain.",
                  "language": "English"
                },
                {
                  "word": "invade",
                  "simple_definition": "To go into with force.",
                  "language": "English"
                }
              ],
              "children": [
                {
                  "entry": "*en",
                  "type": "root",
                  "definition": "In.",
                  "origin_language": {
                    "iso_639_3_code": "ine",
                    "full_language_name": "Proto-Indo-European"
                  },
                  "notes": "PIE root meaning 'in'.",
                  "examples": [
                    {
                      "word": "in",
                      "simple_definition": "Inside.",
                      "language": "English"
                    },
                    {
                      "word": "enter",
                      "simple_definition": "To come into.",
                      "language": "English"
                    }
                  ]
                }
              ]
            },
            {
              "entry": "serere",
              "type": "root",
              "definition": "To join, attach, put in place, string together.",
              "origin_language": {
                "iso_639_3_code": "lat",
                "full_language_name": "Latin"
              },
              "examples": [
                {
                  "word": "series",
                  "simple_definition": "A number of similar things in a row.",
                  "language": "English"
                },
                {
                  "word": "exert",
                  "simple_definition": "To put forth effort (related to 'serere' through the idea of stretching or putting forth).",
                  "language": "English"
                }
              ],
              "children": [
                {
                  "entry": "*ser-",
                  "type": "root",
                  "definition": "To string together.",
                  "origin_language": {
                    "iso_639_3_code": "ine",
                    "full_language_name": "Proto-Indo-European"
                  },
                  "notes": "PIE root related to joining and arranging in a line.",
                  "examples": [
                    {
                      "word": "sermon",
                      "simple_definition": "A string of religious words.",
                      "language": "English"
                    },
                    {
                      "word": "assert",
                      "simple_definition": "To declare strongly (related to 'joining' words together).",
                      "language": "English"
                    }
                  ]
                }
              ]
            }
          ]
        }
      ]
    },
    {
      "entry": "-ion",
      "type": "suffix",
      "definition": "Action or process; state or condition (forming nouns from verbs).",
      "origin_language": {
        "iso_639_3_code": "eng",
        "full_language_name": "English"
      },
      "examples": [
        {
          "word": "creation",
          "simple_definition": "The action of creating.",
          "language": "English"
        },
        {
          "word": "action",
          "simple_definition": "The process of doing something.",
          "language": "English"
        }
      ],
      "children": [
        {
          "entry": "-ionem",
          "type": "suffix",
          "definition": "Accusative suffix forming nouns from verbs.",
          "origin_language": {
            "iso_639_3_code": "lat",
            "full_language_name": "Latin"
          },
          "notes": "Latin accusative suffix, ancestor of English '-ion'.",
          "examples": [
            {
              "word": "(Not directly applicable as -ionem is a suffix form, but consider related Latin nouns like) actio",
              "simple_definition": "Action.",
              "language": "Latin"
            }
          ],
          "children": [
            {
              "entry": "-tio",
              "type": "suffix",
              "definition": "Suffix forming nouns from verbs.",
              "origin_language": {
                "iso_639_3_code": "lat",
                "full_language_name": "Latin"
              },
              "notes": "Latin suffix '-tio', further ancestor of English '-ion'.",
              "children": [
                {
                  "entry": "*-tiō",
                  "type": "root",
                  "definition": "Suffix for noun formation from verbs.",
                  "origin_language": {
                    "iso_639_3_code": "ine",
                    "full_language_name": "Proto-Indo-European"
                  },
                  "notes": "PIE suffix contributing to noun formation from verbs.",
                  "examples": [
                    {
                      "word": "(Conceptual, as PIE is reconstructed) Consider the concept of 'action' across Indo-European languages.",
                      "simple_definition": "The general idea of a verbal noun ending.",
                      "language": "Proto-Indo-European concept"
                    }
                  ]
                }
              ]
            }
          ]
        }
      ]
    }
  ]
}
```