# Installation Guide for URL to Markdown Plugin

This guide will help you install and use the URL to Markdown plugin for Logseq.

## Prerequisites

- Logseq installed on your computer
- Basic knowledge of how to use Logseq plugins

## Installation Methods

### Method 1: From Logseq Marketplace (Recommended)

1. Open Logseq
2. Go to `Settings` (three dots in the top-right corner) > `Plugins` > `Marketplace`
3. Search for "URL to Markdown"
4. Click "Install"
5. Restart Logseq if prompted

### Method 2: Manual Installation

1. Download the latest release from the GitHub repository
2. Extract the zip file to a location on your computer
3. In Logseq, go to `Settings` > `Plugins` > `Load unpacked plugin`
4. Navigate to the extracted folder and select it
5. The plugin should now be loaded and ready to use

## Building from Source

If you want to build the plugin from source:

1. Make sure you have [Bun](https://bun.sh/) installed
2. Clone the repository or download the source code
3. Navigate to the plugin directory in your terminal
4. Run the build script:
   ```
   ./build.sh
   ```
   
   Or manually:
   ```
   bun install
   bun run build
   ```
5. This will install dependencies and build the plugin
6. The built plugin will be in the `dist` directory
7. Follow Method 2 above to load the unpacked plugin

## Development

For development with auto-reload:

```
bun run dev
```

This will watch for changes and rebuild the plugin automatically.

## Usage

Once installed, you can use the plugin in two ways:

1. **Slash Command**: Type `/Convert URLs to Markdown` in a block that contains URLs
2. **Toolbar Button**: Click the "URL→MD" button in the toolbar while your cursor is in a block that contains URLs

The plugin will automatically fetch the title of each URL and convert it to markdown format.

## Troubleshooting

- If the plugin doesn't appear in the toolbar, try restarting Logseq
- If URLs aren't being converted, check your internet connection as the plugin needs to fetch the title from the URL
- For any issues, please report them on the GitHub repository

## License

This plugin is licensed under the MIT License.
