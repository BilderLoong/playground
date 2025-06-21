# URL to Markdown - Logseq Plugin

A Logseq plugin that converts URLs in blocks to markdown format with the title of the URL.

## Features

- Automatically converts URLs to markdown format `[title of url](url)`
- Works with URLs starting with http://, https://, or www.
- Provides a slash command: `/Convert URLs to Markdown`
- Adds a toolbar button for quick access
- Built with TypeScript and Bun for improved performance and type safety

## Installation

### From Logseq Marketplace

1. Open Logseq
2. Go to `Settings` > `Plugins` > `Marketplace`
3. Search for "URL to Markdown"
4. Click "Install"

### Manual Installation

1. Download the latest release from the [releases page](https://github.com/yourusername/logseq-url-to-markdown/releases)
2. Extract the zip file
3. In Logseq, go to `Settings` > `Advanced` > `Developer Mode` and enable it.
4. In Logseq, go to `Settings` > `Plugins` > `Load unpacked plugin`
5. Select the extracted folder

## Usage

There are two ways to use this plugin:

1. **Slash Command**: Type `/Convert URLs to Markdown` in a block that contains URLs
2. **Toolbar Button**: Click the "URL→MD" button in the toolbar while your cursor is in a block that contains URLs

The plugin will automatically fetch the title of each URL and convert it to markdown format.

## Example

Before:
```
Check out this cool website: https://example.com
```

After:
```
Check out this cool website: [Example Domain](https://example.com)
```

## Development

### Prerequisites

- [Bun](https://bun.sh/) - Fast JavaScript runtime and package manager

### Setup

1. Clone the repository
2. Install dependencies: `bun install`
3. Build the plugin: `bun run build`
4. For development with auto-reload: `bun run dev`
5. Load the unpacked plugin in Logseq

## Technical Details

This plugin is built with:
- TypeScript for type safety and better developer experience
- Bun for fast builds and modern JavaScript features
- Undici for efficient HTTP requests

## License

MIT
