type tokenType = "string" | "paren" | "number" | "name";

interface Token {
  type: tokenType;
  value: string;
}

namespace LispAST {
  export type primaryNodeType = "NumberLiteral" | "StringLiteral";

  export type PrimaryNode = {
    type: primaryNodeType;
    value: string;
    _context?: any;
  };

  export type Node = PrimaryNode | CallNode | RootNode;
  export type ParentNode = CallNode | RootNode;

  export interface CallNode {
    type: "CallExpression";
    name: Token["value"];
    params: Node[];
    _context?: any;
  }

  export interface RootNode {
    type: "Program";

    body: Array<Node>;

    _context?: any;
  }
  export type NodeType = LispAST.Node["type"];
}

namespace JavaScriptAST {
  type primaryNodeType = "NumberLiteral" | "StringLiteral";

  export type Node =
    | RootNode
    | ExpressionStatement
    | CallExpression
    | Identifier
    | PrimaryNode<primaryNodeType>;

  interface BaseNode<T> {
    type: T;
  }

  // type Node
  export interface RootNode {
    type: "Program";
    body: Array<Node>;
  }

  export interface ExpressionStatement extends BaseNode<"ExpressionStatement"> {
    expression: CallExpression;
  }

  export interface CallExpression extends BaseNode<"CallExpression"> {
    callee: Identifier;
    arguments: Node[];
  }

  export interface Identifier extends BaseNode<"Identifier"> {
    name: string;
  }

  export type PrimaryNode<T extends primaryNodeType> = {
    type: T;
    value: string;
  };
}

/**
 * @param input - The string representation of Lisp code.
 *
 * @return A tokens array.
 */
export function tokenizer(input: string) {
  let isInDoubleQuote = false;
  let value = "";
  const tokens: Token[] = [...input].reduce((pre, char, index, arr) => {
    if (char === '"') {
      // If already in double quote, then we are exiting double quote.
      if (isInDoubleQuote) {
        isInDoubleQuote = false;

        const result = [
          ...pre,
          {
            type: "string",
            value,
          },
        ];

        value = "";
        return result;
      }

      // First double quote, so we are entering double quote.
      isInDoubleQuote = true;
      return pre;
    }

    if (isInDoubleQuote) {
      value += char;
      return pre;
    }

    if (char === "(") {
      return [
        ...pre,
        {
          type: "paren",
          value: "(",
        },
      ];
    }

    if (char === ")") {
      return [
        ...pre,
        {
          type: "paren",
          value: ")",
        },
      ];
    }

    const spaceRegex = /\s/;
    // Ignore blank space.
    if (spaceRegex.test(char)) {
      return pre;
    }

    const nextChar = arr[index + 1];
    const numberRegex = /[0-9]/;
    if (numberRegex.test(char)) {
      // If next char is also a number, concatenate them.
      if (numberRegex.test(nextChar)) {
        value += char;
        return pre;
      }

      const tmp = value + char;
      // If next char is not a number, push the number token.
      value = ""; // Clear the value for late use.
      return [...pre, { type: "number", value: tmp }];
    }

    const nameRegex = /[a-z]/i;
    if (nameRegex.test(char)) {
      if (nameRegex.test(nextChar || "")) {
        value += char;
        return pre;
      }

      const tmp = value + char;
      value = "";
      return [...pre, { type: "name", value: tmp }];
    }

    throw new Error(`Unexpected token: ${char}`);
  }, [] as Token[]);

  return tokens;
}

export function parser(tokens: Token[]): LispAST.RootNode {
  let current = 0;

  /**
   * @returns The node representation of a range of tokens.
   * @remarks After each call to the walk() function the current pointer
   *          will point to the next token ("unwalked").
   */
  function walk(): LispAST.Node {
    let token = tokens[current];

    if (token.type === "string") {
      current++;
      return { type: "StringLiteral", value: token.value };
    }

    if (token.type === "number") {
      current++;
      return { type: "NumberLiteral", value: token.value };
    }

    if (token.type === "paren" && token.value === "(") {
      // Current token is the name of this call expression.
      token = tokens[++current];

      const callNode: LispAST.CallNode = {
        type: "CallExpression",
        name: token.value,
        params: [],
      };

      // The first param of current procedure.
      token = tokens[++current];

      while (
        token.type !== "paren" ||
        (token.type === "paren" && token.value === "(")
      ) {
        callNode.params.push(walk());
        // At this time, `current` point to next unused token.
        token = tokens[current];
      }

      // Skip the ')' token.
      current++;

      return callNode;
    }

    throw new TypeError(`Unexpected token type: ${token.type}`);
  }

  const AST: LispAST.RootNode = {
    type: "Program",
    body: [],
  };

  while (current < tokens.length) {
    AST.body.push(walk());
  }

  return AST;
}

type Visitor = {
  [type in LispAST.NodeType]: {
    enter?: (
      node: type extends LispAST.primaryNodeType
        ? LispAST.PrimaryNode
        : Exclude<LispAST.Node, LispAST.PrimaryNode>,
      parent: LispAST.ParentNode | null
    ) => void;

    exit?: (
      node: type extends LispAST.primaryNodeType
        ? LispAST.PrimaryNode
        : Exclude<LispAST.Node, LispAST.PrimaryNode>,
      parent: LispAST.ParentNode | null
    ) => void;
  };
};

function traverser(ast: LispAST.RootNode, visitor: Visitor) {
  /**
   * @description Traverse all nodes in an array.
   * @param array An array contain @see ASTNode.
   * @param parent The node where the given array locate.
   */
  function traverseNodesInArray(
    array: LispAST.Node[],
    parent: LispAST.ParentNode
  ) {
    array.forEach((node) => {
      traverseNode(node, parent);
    });
  }

  /**
   * @description Traverse the given node recursively.
   */
  function traverseNode(node: LispAST.Node, parent: LispAST.ParentNode | null) {
    const methods = visitor[node.type];

    // Enter the ASTNode.
    // TODO: Don't understand why does the below code got error.
    methods?.enter && methods.enter(node, parent);

    // If the node isn't primary node, in other words, the node isn't the leaves of the AST tree,
    // which means it contain child nodes. We should traverse it's child nodes.
    switch (node.type) {
      case "Program":
        traverseNodesInArray(node.body, node);
        break;
      case "CallExpression":
        traverseNodesInArray(node.params, node);
        break;
      case "NumberLiteral":
      case "StringLiteral":
        break;
    }

    // Exit from the current node.
    methods?.exit && methods.exit(node, parent);
  }

  traverseNode(ast, null);
}

export function transformer(ast: LispAST.RootNode) {
  const newAst: JavaScriptAST.RootNode = {
    type: "Program",
    body: [],
  };

  ast._context = newAst.body;

  const visitor: Visitor = {
    CallExpression: {
      enter: (node, parent) => {
        let expression:
          | JavaScriptAST.CallExpression
          | JavaScriptAST.ExpressionStatement = {
          type: "CallExpression",
          callee: {
            type: "Identifier",
            name: (<LispAST.CallNode>node).name,
          },
          arguments: [],
        };

        node._context = expression.arguments;

        // TODO to be continue.
        if (parent?.type !== "CallExpression") {
          expression = {
            type: "ExpressionStatement",
            expression,
          };
        }

        parent?._context.push(expression);
      },
    },

    NumberLiteral: {
      enter: (node, parent) => {
        const newNode: JavaScriptAST.PrimaryNode<"NumberLiteral"> = {
          type: "NumberLiteral",
          value: node.value,
        };

        parent?._context.push(newNode);
      },
    },

    StringLiteral: {
      enter: (node, parent) => {
        const newNode: JavaScriptAST.PrimaryNode<"StringLiteral"> = {
          type: "StringLiteral",
          value: node.value,
        };

        parent?._context.push(newNode);
      },
    },
  };

  traverser(ast, visitor);

  return newAst;
}

export function codeGenerator(node: JavaScriptAST.Node): string {
  switch (node.type) {
    case "Program":
      return node.body.map(codeGenerator).join("\n");

    case "ExpressionStatement":
      return codeGenerator(node.expression) + ";";

    case "CallExpression":
      return `${codeGenerator(node.callee)}(${node.arguments
        .map(codeGenerator)
        .join(", ")})`;

    case "Identifier":
      return node.name;

    case "NumberLiteral":
      return node.value;

    case "StringLiteral":
      return `"${node.value}"`;
  }
}

export function compiler(input: string): string {
  const tokens = tokenizer(input);
  const lispAst = parser(tokens);
  const newAst = transformer(lispAst);
  const output = codeGenerator(newAst);
  return output;
}

console.log(compiler("(bar 1 (foo 2 3))"));
console.log(compiler("1 1 1 1 "));
