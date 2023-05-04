import path from "path";
import HtmlWebpackPlugin from "html-webpack-plugin";
import webpack from "webpack";
import { ConsoleLogOnBuildWebpackPlugin } from "./plugins/index";

import "webpack-dev-server";

const config: webpack.Configuration = {
  mode: "development",
  entry: {
    page1: "./src/page1.js",
    page2: "./src/page2.js",
  },
  output: {
    filename: "[name].bundle.js",
    path: path.resolve(__dirname, "dist"),
    // https://webpack.js.org/guides/output-management/#cleaning-up-the-dist-folder
    clean: true,
  },
  plugins: [
    new ConsoleLogOnBuildWebpackPlugin(),
    new HtmlWebpackPlugin({
      filename: "page1.html",
      chunks: ["page1"],
    }),
    new HtmlWebpackPlugin({
      filename: "page2.html",
      chunks: ["page2"],
    }),
  ],
  devServer: {
    // contentBase: path.resolve(__dirname, "dist"),
    // static: "./dist",
    port: 8081,
  },
};

export default config;
