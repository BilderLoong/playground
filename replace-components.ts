import { glob, globSync, globStream, globStreamSync, Glob } from "glob";
import z from "zod";
import yaml from "js-yaml";
import fs from "node:fs/promises";
import gitignoreParser from "parse-gitignore";
import path from "path";
import { minimatch } from "minimatch";

async function main() {
  const filePathPartInMenuPackage: string[] = z
    .object({
      list: z.array(z.string()),
    })
    .parse(
      yaml.load(
        await fs.readFile(
          "/Users/birudo/Project/menuorder-new-rms-h5-0/menu-package-file-list.yml",
          "utf8"
        )
      )
    ).list;
  // console.log(filePathPartInMenuPackage)
  const filePatterInMenuPackage: string[] = z
    .object({
      ignores: z.array(z.string()),
    })
    .parse(
      yaml.load(
        await fs.readFile(
          "/Users/birudo/Project/menuorder-new-rms-h5-0/rules/menu-package-import-TS.yml",
          "utf8"
        )
      )
    ).ignores;
  const cwd = "/Users/birudo/Project/menuorder-new-rms-h5-0";
  const ignores = gitignoreParser(await fs.readFile(".gitignore")).patterns;

  const allFiles = (
    await glob(["apps/menuorder/**/*.json", "apps/components/**/*.json"], {
      cwd,
      ignore: [
        ...filePatterInMenuPackage,
        ...ignores,
        "**/src/limo/**",
        "**/limo_temp/**",
        "**/node_modules/**",
        "**/.remote/**",
        "**/dist/**",
      ],
    })
  ).map((e) => path.join(cwd, e));

  allFiles.forEach((e) => {
    (async function () {
      const fileData = await fs.readFile(e, "utf8");
      try {
        // Parse the JSON data into an object
        const componentBefore = componentsSchema.parse(JSON.parse(fileData));
        const usedComponents = componentBefore.usingComponents;

        if (!usedComponents) {
          return;
        }

        let shouldReplace = false;
        const newUsingComponents = Object.fromEntries(
          Object.entries(usedComponents).map(([key, val]) => {
            const matchedMenuPackageComponentPathPart =
              filePathPartInMenuPackage.find((e) => val.includes(e));
            const containAlias = val.includes("@");
            const isLimoContainerComponent =
              path.basename(path.dirname(e)) === "limo-container";

            if (
              !matchedMenuPackageComponentPathPart ||
              !containAlias
              ||
              isLimoContainerComponent
            ) {
              return [key, val];
            }

            const replaceRes = `@main/${matchedMenuPackageComponentPathPart}`;
            shouldReplace = true;

            return [key, replaceRes];
          })
        );

        if (shouldReplace) {
          fs.writeFile(
            e,
            JSON.stringify(
              componentsSchema.parse({
                ...componentBefore,
                usingComponents: {
                  ...componentBefore.usingComponents,
                  ...newUsingComponents,
                },
              }),
              null,
              2
            )
          );
        }
      } catch (error) {
        console.error("Error reading file:", error);
        console.error({ path: e });
        return;
      }
    })();
  });
}

main();

const componentsSchema = z.record(z.any()).and(
  z.object({
    usingComponents: z.record(z.string()).optional(),
  })
);
