import { Compiler } from "webpack";
const pluginName = "ConsoleLogOnBuildWebpackPlugin";

export class ConsoleLogOnBuildWebpackPlugin {
  apply(compiler: Compiler) {
    compiler.hooks.run.tap(pluginName, (compilation) => {
      console.trace("123");
      console.log("The webpack build process is starting!");
    });
  }
}
