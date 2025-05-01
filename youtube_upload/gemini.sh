#!/bin/bash

# --- Configuration ---
PLAYLIST_NAME="Go pro back up"
VIDEO_PRIVACY="unlisted"
ARCHIVE_SUBDIR="archive"
MAX_RETRIES=3
RETRY_DELAY_SECONDS=60 # Initial delay, will double on each retry

# --- Function Definitions ---

# Check if required commands exist
check_commands() {
    local missing=0
    for cmd in youtube-upload ffprobe gzip base64 dirname basename mkdir mv find; do
        if ! command -v "$cmd" &> /dev/null; then
            echo "Error: Required command '$cmd' not found in PATH." >&2
            missing=1
        fi
    done
    if [[ $missing -eq 1 ]]; then
        echo "Please install the missing commands and ensure they are in your PATH." >&2
        exit 1
    fi
}

# Display help message
usage() {
    echo "Usage: $0 <video_file.mp4 | directory_path>"
    echo ""
    echo "Uploads MP4 videos to YouTube."
    echo "  - Titles are derived from filenames."
    echo "  - Uploaded videos are moved to an '$ARCHIVE_SUBDIR/' subdirectory."
    echo "  - ffprobe metadata is added to the description (gzipped & base64 encoded)."
    echo "  - Uploads to playlist '$PLAYLIST_NAME' with '$VIDEO_PRIVACY' privacy."
    echo "  - Retries on failure up to $MAX_RETRIES times."
    echo ""
    echo "Requires: youtube-upload, ffmpeg (for ffprobe), gzip, base64."
    echo "Ensure your youtube-upload OAuth is configured (~/.client_secrets.json)."
}

# --- Main Script Logic ---

# Check prerequisites
check_commands

# Check for input argument
if [[ $# -ne 1 ]]; then
    usage
    exit 1
fi

INPUT_PATH="$1"
VIDEO_FILES=()
TARGET_DIR="" # Directory where the archive folder will be created

# Determine if input is a file or directory and populate VIDEO_FILES array
if [[ -d "$INPUT_PATH" ]]; then
    TARGET_DIR="$INPUT_PATH"
    echo "Scanning directory: $TARGET_DIR"
    # Use find with -print0 and read -d '' for safe handling of filenames
    while IFS= read -r -d $'\0' file; do
        VIDEO_FILES+=("$file")
    done < <(find "$TARGET_DIR" -maxdepth 1 -type f \( -name '*.mp4' -o -name '*.MP4' \) -print0)
    if [[ ${#VIDEO_FILES[@]} -eq 0 ]]; then
         echo "No .mp4 or .MP4 files found directly in '$TARGET_DIR'."
         exit 0
    fi
elif [[ -f "$INPUT_PATH" ]]; then
    # Check if the file is actually an MP4 video
    if [[ "$INPUT_PATH" == *.mp4 || "$INPUT_PATH" == *.MP4 ]]; then
        VIDEO_FILES+=("$INPUT_PATH")
        TARGET_DIR=$(dirname "$INPUT_PATH")
        echo "Processing single file: $INPUT_PATH"
    else
        echo "Error: Input file '$INPUT_PATH' is not an .mp4 or .MP4 file." >&2
        exit 1
    fi
else
    echo "Error: Input path '$INPUT_PATH' is not a valid file or directory." >&2
    exit 1
fi

# Define the archive directory path
ARCHIVE_DIR_PATH="$TARGET_DIR/$ARCHIVE_SUBDIR"

# --- Process Videos ---
TOTAL_FILES=${#VIDEO_FILES[@]}
UPLOADED_COUNT=0
FAILED_FILES=()

echo "Found $TOTAL_FILES video(s) to process."
echo "Playlist: '$PLAYLIST_NAME', Privacy: '$VIDEO_PRIVACY'"
echo "Uploaded files will be moved to: $ARCHIVE_DIR_PATH/"
echo "---"

for (( i=0; i<${#VIDEO_FILES[@]}; i++ )); do
    FILE_PATH="${VIDEO_FILES[$i]}"
    FILENAME=$(basename "$FILE_PATH")
    TITLE="${FILENAME%.*}" # Get filename without extension for the title
    CURRENT_COUNT=$((i + 1))

    echo "[${CURRENT_COUNT}/${TOTAL_FILES}] Processing: $FILENAME"

    # 1. Get metadata with ffprobe, gzip, and base64 encode it
    echo "  Extracting metadata..."
    METADATA_JSON=$(ffprobe -v quiet -print_format json -show_format -show_streams "$FILE_PATH")
    if [[ $? -ne 0 ]]; then
        echo "  Error: ffprobe failed for '$FILENAME'. Skipping metadata." >&2
        DESCRIPTION="Metadata extraction failed."
    else
        METADATA_GZ_B64=$(echo "$METADATA_JSON" | gzip -c | base64 -w 0) # -w 0 prevents line wrapping in base64 output
        DESCRIPTION="Video metadata (ffprobe JSON, gzipped, base64 encoded):${METADATA_GZ_B64}"
        echo "  Metadata extracted and encoded."
    fi

    # 2. Attempt upload with retry logic
    echo "  Uploading '$TITLE'..."
    RETRY_COUNT=0
    UPLOAD_SUCCESS=false
    CURRENT_RETRY_DELAY=$RETRY_DELAY_SECONDS

    while [[ $RETRY_COUNT -lt $MAX_RETRIES && "$UPLOAD_SUCCESS" = false ]]; do
        if [[ $RETRY_COUNT -gt 0 ]]; then
            echo "  Retrying upload ($((RETRY_COUNT + 1))/$MAX_RETRIES) in ${CURRENT_RETRY_DELAY}s..."
            sleep "$CURRENT_RETRY_DELAY"
            CURRENT_RETRY_DELAY=$((CURRENT_RETRY_DELAY * 2)) # Exponential backoff
        fi

        # Execute the upload command
        # Capture stdout (which might contain the video ID) and stderr
        UPLOAD_OUTPUT=$(youtube-upload \
          --title="$TITLE" \
          --description="$DESCRIPTION" \
          --playlist="$PLAYLIST_NAME" \
          --privacy="$VIDEO_PRIVACY" \
          "$FILE_PATH" 2>&1) # Redirect stderr to stdout to capture all output

        UPLOAD_EXIT_CODE=$?

        if [[ $UPLOAD_EXIT_CODE -eq 0 ]]; then
            UPLOAD_SUCCESS=true
            echo "  Successfully uploaded: $FILENAME"
             # Optional: Print the video ID if youtube-upload outputs it
            VIDEO_ID=$(echo "$UPLOAD_OUTPUT" | grep -Eo '[a-zA-Z0-9_-]{11}') # Basic regex for YouTube ID
             if [[ -n "$VIDEO_ID" ]]; then
                 echo "  Video ID: $VIDEO_ID"
             else
                 # If no ID found, print the raw output for debugging (might vary with youtube-upload version)
                 echo "  Upload command output: $UPLOAD_OUTPUT"
             fi
        else
            echo "  Upload failed for '$FILENAME' (Exit Code: $UPLOAD_EXIT_CODE). Possible API limit or network error."
            echo "  youtube-upload output: $UPLOAD_OUTPUT" # Show error output
            RETRY_COUNT=$((RETRY_COUNT + 1))
        fi
    done

    # 3. Move file if upload succeeded
    if [[ "$UPLOAD_SUCCESS" = true ]]; then
        # Create archive directory if it doesn't exist
        mkdir -p "$ARCHIVE_DIR_PATH"
        if [[ $? -ne 0 ]]; then
            echo "  Error: Could not create archive directory '$ARCHIVE_DIR_PATH'. File not moved." >&2
            # Decide if this counts as a failure or just a warning
            FAILED_FILES+=("$FILENAME (Move Failed)") # Add note about move failure
        else
            echo "  Archiving to $ARCHIVE_DIR_PATH/"
            mv "$FILE_PATH" "$ARCHIVE_DIR_PATH/"
            if [[ $? -ne 0 ]]; then
                echo "  Error: Failed to move '$FILENAME' to archive directory." >&2
                FAILED_FILES+=("$FILENAME (Move Failed)") # Add note about move failure
            else
                 UPLOADED_COUNT=$((UPLOADED_COUNT + 1))
                 echo "  Successfully archived."
            fi
        fi
    else
        echo "  Upload failed permanently for '$FILENAME' after $MAX_RETRIES attempts. Skipping archival."
        FAILED_FILES+=("$FILENAME (Upload Failed)")
    fi
    echo "---" # Separator between files
done

# --- Final Summary ---
echo "========================================="
echo "Batch Upload Summary"
echo "========================================="
echo "Total videos processed: $TOTAL_FILES"
echo "Successfully uploaded and archived: $UPLOADED_COUNT"
FAILED_COUNT=${#FAILED_FILES[@]}
echo "Failed attempts (upload or move): $FAILED_COUNT"

if [[ $FAILED_COUNT -gt 0 ]]; then
    echo "-----------------------------------------"
    echo "Files with issues:"
    printf "  - %s\n" "${FAILED_FILES[@]}"
    echo "-----------------------------------------"
fi

echo "Script finished."

# Exit with success only if all files were processed without permanent errors
if [[ $FAILED_COUNT -eq 0 ]]; then
    exit 0
else
    exit 1 # Indicate that some operations failed
fi