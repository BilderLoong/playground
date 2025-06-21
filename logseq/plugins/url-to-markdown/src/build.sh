#!/bin/bash

# Build script for URL to Markdown Logseq plugin

echo "Installing dependencies..."
bun install

echo "Building plugin..."
mkdir -p dist
bun run build

echo "Build complete! The plugin is now ready to use."
echo "You can find the built plugin in the 'dist' directory."
