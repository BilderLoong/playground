## Prompt that generates this script file

[Python Script for YouTube Vide | Google AI Studio](https://aistudio.google.com/prompts/17AfmthHxOBgf2Q2vLLQehX-r7UV0IbBv)


# YouTube Batch Uploader Script (`youtube_batch_upload.sh`)

This Bash script automates the process of uploading multiple MP4 videos to a specific YouTube channel using the `tokland/youtube-upload` command-line tool.

It simplifies uploading batches of videos by automatically setting titles, embedding technical metadata in the description, organizing uploaded files, and handling common upload issues.

## Features

*   **Batch Upload:** Upload all `.mp4` / `.MP4` files from a specified directory or upload a single specified video file.
*   **Automatic Titling:** Sets the YouTube video title based on the video's filename (excluding the extension).
*   **Metadata in Description:** Extracts detailed video metadata using `ffprobe`, compresses it using `gzip`, encodes it using `base64`, and embeds it within the video description.
*   **Archiving:** Automatically moves successfully uploaded videos to an `archive` subdirectory within the source directory to prevent re-uploading.
*   **Playlist Assignment:** Uploads videos directly to a predefined playlist (configurable).
*   **Privacy Control:** Sets the privacy status of uploaded videos (configurable, defaults to `unlisted`).
*   **Progress Reporting:** Displays the progress of the upload process (which file is being processed, upload status).
*   **Error Handling:** Implements a basic retry mechanism with exponential backoff to handle intermittent network errors or YouTube API rate limits.
*   **Summary Report:** Provides a summary of successful uploads and any failures at the end.

## Requirements

1.  **Bash:** A standard Unix-like shell.
2.  **`youtube-upload`:** The core tool used for interacting with the YouTube API.
    *   Installation and documentation: [GitHub - tokland/youtube-upload](https://github.com/tokland/youtube-upload)
3.  **`ffmpeg`:** Required for the `ffprobe` command to extract video metadata. Most Linux distributions provide this via their package manager (e.g., `sudo apt install ffmpeg` or `sudo yum install ffmpeg`). macOS users can use Homebrew (`brew install ffmpeg`).
4.  **`gzip`:** Standard compression utility, usually pre-installed on Linux/macOS.
5.  **`base64`:** Standard encoding utility, usually pre-installed on Linux/macOS.
6.  youtube-upload need `~/.client_secrets.json` file which can be generated from [Client ID for Desktop – Google Auth Platform – YouTube Data API – Google Cloud console](https://console.cloud.google.com/auth/clients/512800420159-5f6udpgkb4q0tggrk27ek9nnk51l2vju.apps.googleusercontent.com?project=youtube-data-api-347207)

## Setup

1.  **Install Dependencies:** Ensure `youtube-upload`, `ffmpeg`, `gzip`, and `base64` are installed and accessible in your system's PATH.
    ```bash
    # Example using pip for youtube-upload and apt for ffmpeg (Debian/Ubuntu)
    pip install --upgrade google-api-python-client oauth2client progressbar2
    # Install youtube-upload itself (e.g., from source or pip if available)
    sudo apt update && sudo apt install ffmpeg gzip coreutils # coreutils provides base64
    ```
    *(Adapt package manager commands based on your OS)*
2.  **Configure OAuth:** Follow the [youtube-upload setup guide](https://github.com/tokland/youtube-upload#setup) to obtain your `client_secrets.json` and perform the initial authorization.
3.  **Save the Script:** Save the Bash script code to a file named `youtube_batch_upload.sh`.
4.  **Make Executable:** Open your terminal and grant execute permissions to the script:
    ```bash
    chmod +x youtube_batch_upload.sh
    ```

## Configuration (Inside the Script)

You can modify the following variables at the top of the `youtube_batch_upload.sh` script to change its behavior:

*   `PLAYLIST_NAME`: The name of the YouTube playlist to add videos to (default: `"Go pro back up"`). If the playlist doesn't exist, `youtube-upload` will usually create it.
*   `VIDEO_PRIVACY`: The privacy setting for the uploaded videos (`public`, `unlisted`, or `private`) (default: `"unlisted"`).
*   `ARCHIVE_SUBDIR`: The name of the subdirectory where successfully uploaded videos will be moved (default: `"archive"`).
*   `MAX_RETRIES`: The maximum number of times the script will retry uploading a specific file if it fails (default: `3`).
*   `RETRY_DELAY_SECONDS`: The initial delay (in seconds) before the first retry. This delay doubles for each subsequent retry (exponential backoff) (default: `60`).

## Usage

Execute the script from your terminal, providing the path to either a directory containing MP4 videos or a single MP4 video file.

**Upload all `.mp4`/`.MP4` videos in a directory:**

```bash
./youtube_batch_upload.sh /path/to/your/video_directory