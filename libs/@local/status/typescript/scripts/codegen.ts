/** @todo - extract this into a generalised shared @local/* package for generating things through quicktype */

import { existsSync, lstatSync, rmSync } from "node:fs";
import { mkdir, readdir, writeFile } from "node:fs/promises";
import * as path from "node:path";
import { argv } from "node:process";

import execa from "execa";
import snakeCase from "lodash/snakeCase";
import yargs from "yargs";

const removeOldGenDirs = (outDir: string) => {
  const outDirParent = path.dirname(outDir);

  if (!existsSync(outDirParent)) {
    throw new Error(
      `Couldn't find parent of output directory, are you sure you're running in the correct working directory? Input dir was: ${outDir}, resolved parent was: ${outDirParent}`,
    );
  }
  if (!lstatSync(outDirParent).isDirectory()) {
    throw new Error("Expected parent of output directory to be a directory.");
  }

  rmSync(outDir, { recursive: true, force: true });
};

async function* getFiles(dir: string): AsyncGenerator<string> {
  const dirEntries = await readdir(dir, { withFileTypes: true });
  for (const dirEntry of dirEntries) {
    const resolvedChildPath = path.resolve(dir, dirEntry.name);
    if (dirEntry.isDirectory()) {
      yield* getFiles(resolvedChildPath);
    } else {
      yield resolvedChildPath;
    }
  }
}

const RUST_MODULE_TEMPLATE_STRING = `//! This is an autogenerated module declaration file. All sub-modules were produced by \`quicktype\`\n`;

const postProcessRustDir = async (dir: string) => {
  const moduleNames = [];

  for (const childDirEntry of await readdir(dir, { withFileTypes: true })) {
    const childDirEntryPath = path.join(dir, childDirEntry.name);

    if (childDirEntry.isDirectory()) {
      // If it has a Rust file we need to keep traversing
      if (
        (await readdir(childDirEntryPath)).findIndex((nestedChildDirEntry) => {
          return path.extname(nestedChildDirEntry) === ".rs";
        }) !== -1
      ) {
        await postProcessRustDir(childDirEntryPath);
      }
    }
    const moduleName = path.parse(childDirEntryPath).name;
    moduleNames.push(moduleName);
  }

  const moduleContents = moduleNames.reduce((contents, moduleName) => {
    return contents.concat(`pub mod ${moduleName};\n`);
  }, RUST_MODULE_TEMPLATE_STRING);

  // We stick to the outdated format of `mod.rs`
  // (https://doc.rust-lang.org/reference/items/modules.html#module-source-filenames)
  //
  // As otherwise the generated modules are not encapsulated within the generated directory and we would need to create
  // a `gen.rs` or similar module in the _parent_ of the generated directory
  await writeFile(path.join(dir, "mod.rs"), moduleContents);
};

/**
 *  @todo - Consider using `quicktype-core` and orchestrating ourselves from TS instead of calling
 *    the CLI (https://blog.quicktype.io/customizing-quicktype/)
 */
const codegen = async ({
  typeDefsDir,
  rustOutputDir,
  jsonSchemaOutputDir,
}: {
  typeDefsDir: string;
  rustOutputDir?: string;
  jsonSchemaOutputDir?: string;
}) => {
  for await (const filePath of getFiles(typeDefsDir)) {
    const fileParentStructure = path.relative(
      typeDefsDir,
      path.dirname(filePath),
    );
    const fileName = path.parse(filePath).name;
    const fileExtension = path.extname(filePath);

    const skip =
      (fileName === "package" && fileExtension === ".json") ||
      (fileName.toLowerCase().startsWith("license") && fileExtension !== ".ts");

    if (fileExtension === ".ts") {
      if (rustOutputDir) {
        const rustParentsPath = path.join(rustOutputDir, fileParentStructure);
        await mkdir(rustParentsPath, { recursive: true });

        const rustOutputPath = path.join(
          rustParentsPath,
          `${snakeCase(fileName)}.rs`,
        );

        await execa("quicktype", [
          filePath,
          "-o",
          rustOutputPath,
          "--no-combine-classes",
          "--visibility",
          "crate",
          "--derive-debug",
          "--edition-2018",
        ]);
      }

      if (jsonSchemaOutputDir) {
        const jsonSchemaParentsPath = path.join(
          jsonSchemaOutputDir,
          fileParentStructure,
        );
        await mkdir(jsonSchemaParentsPath, { recursive: true });

        const jsonSchemaOutputPath = path.join(
          jsonSchemaParentsPath,
          `${snakeCase(fileName)}.json`,
        );

        await execa("quicktype", [
          filePath,
          "--lang",
          "schema",
          "-o",
          jsonSchemaOutputPath,
          "--no-combine-classes",
        ]);
      }
    } else if (skip) {
      console.info(`Skipping ${fileName}${fileExtension} file`);
      continue;
    } else {
      throw new Error(`Unsupported quicktype input format: ${fileExtension}`);
    }
  }

  if (rustOutputDir) {
    await postProcessRustDir(rustOutputDir);
  }
};

void (async () => {
  const args = yargs(argv.slice(2))
    .usage(
      "Usage: $0 <entry-point-dir> -r [rust-out-dir] -j [json-schema-out-dir]",
    )
    .positional("entry-point-dir", {
      describe:
        "The directory to recursively search for TypeScript type definitions to generate code from",
      type: "string",
      normalize: true,
    })
    .demandCommand(1)
    .options({
      r: {
        alias: "rust-out-dir",
        describe: "The directory to output the generated Rust files to",
        type: "string",
        nargs: 1,
        normalize: true,
      },
      j: {
        alias: "json-schema-out-dir",
        describe: "The directory to output the generated JSON Schema files to",
        type: "string",
        nargs: 1,
        normalize: true,
      },
    })
    .help("h")
    .alias("h", "help")
    .check((argsToCheck) => {
      if (!argsToCheck.r && !argsToCheck.j) {
        throw new Error(
          "At least one of `rust-out-dir` or `json-schema-out-dir` must be specified",
        );
      }
      return true;
    })
    .parseSync();

  const [entryPointDir] = args._;
  const { r: rustOutDir, j: jsonSchemaOutDir } = args;

  if (rustOutDir) {
    console.log("Removing old directories for generated Rust files");
    removeOldGenDirs(rustOutDir);
  }

  if (jsonSchemaOutDir) {
    console.log("Removing old directories for generated JSON Schema files");
    removeOldGenDirs(jsonSchemaOutDir);
  }

  console.log("Running codegen");
  await codegen({
    typeDefsDir: entryPointDir as string,
    rustOutputDir: rustOutDir,
    jsonSchemaOutputDir: jsonSchemaOutDir,
  });
})();
