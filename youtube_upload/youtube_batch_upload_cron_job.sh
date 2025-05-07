#!/bin/bash

# Set a robust PATH as cron jobs may have a limited one
export PATH="/usr/local/bin:/usr/bin:/bin" # Adjust if necessary for your system commands (kill, cat, etc.)
export PATH="/Users/birudo/.local/bin:/opt/homebrew/bin:/usr/local/bin:/usr/bin:/bin:/Users/birudo/.pyenv/shims/:$PATH"

# --- Configuration ---
# Define the path to the PID file
# Choose a hidden file in your home directory for uniqueness per user
PID_FILE="$HOME/log/.youtube_upload_batch_upload.pid"

# Define the actual command you want to run
TARGET_SCRIPT="/Users/birudo/Projects/playground/youtube_upload/youtube_batch_upload.sh"
TARGET_ARGS="$HOME/Movies/GoPro"

# How long to wait (in seconds) after sending SIGTERM before sending SIGKILL
KILL_TIMEOUT=30

# Optional: Define a log file for the wrapper script's output
LOG_FILE="$HOME/logs/youtube_upload_cron.log"
# Ensure the directory exists if you use the log file
mkdir -p "$(dirname "$LOG_FILE")" # Uncomment if using LOG_FILE

# Redirect standard output and error to the log file, or let cron handle emailing
# If you want to see output even when not using a specific LOG_FILE,
# comment out the exec line below. Cron will email you output/errors.
exec > "$LOG_FILE" 2>&1 # Uncomment if using LOG_FILE

# --- Process Management Logic ---

echo "--------------------------------------------------"
echo "$(date): Wrapper script started."

# Check if the PID file exists
if [ -f "$PID_FILE" ]; then
  # Read the PID from the file
  OLD_PID=$(cat "$PID_FILE")

  # Check if a process with that PID is still running
  # kill -0 sends no signal but checks if the process exists and you have permission
  if kill -0 "$OLD_PID" 2>/dev/null; then
    echo "$(date): Previous process (PID: $OLD_PID) is still running."
    echo "$(date): Attempting to kill PID: $OLD_PID"

    # Attempt graceful termination first (SIGTERM is default for kill)
    kill "$OLD_PID"

    # Wait for the process to terminate
    TERMINATED=0
    for i in $(seq 1 $KILL_TIMEOUT); do
      if ! kill -0 "$OLD_PID" 2>/dev/null; then
        echo "$(date): Previous process (PID: $OLD_PID) terminated."
        TERMINATED=1
        break # Process died
      fi
      sleep 1
    done

    # If the process is still running after the timeout, force kill (SIGKILL)
    if [ "$TERMINATED" -eq 0 ]; then
      echo "$(date): Previous process (PID: $OLD_PID) did not terminate gracefully after $KILL_TIMEOUT seconds. Forcing kill (SIGKILL)."
      kill -9 "$OLD_PID"
      # Give it a moment to be killed
      sleep 1
       # Final check to see if it's really gone
       if kill -0 "$OLD_PID" 2>/dev/null; then
         echo "$(date): ERROR: Previous process (PID: $OLD_PID) could not be killed even with SIGKILL. Aborting new start."
         exit 1 # Exit with an error status
       else
         echo "$(date): Previous process (PID: $OLD_PID) killed with SIGKILL."
       fi
    fi

    # Remove the old PID file after handling the previous process
    rm -f "$PID_FILE"
    echo "$(date): Stale/Old PID file removed."

  else
    # PID file exists, but the process is not running (stale PID file)
    echo "$(date): Stale PID file found ($PID_FILE) with PID $OLD_PID. Removing it."
    rm -f "$PID_FILE"
  fi
else
  echo "$(date): No existing PID file found ($PID_FILE)."
fi

# --- Start the new process ---

echo "$(date): Starting new process: $TARGET_SCRIPT $TARGET_ARGS"

# Write the current process's PID (the wrapper script's PID) to the PID file
echo "$$" > "$PID_FILE"

# Set a trap to ensure the PID file is removed when this script exits,
# regardless of how it exits (success, failure, killed)
trap "rm -f '$PID_FILE'; echo \"$(date): PID file $PID_FILE removed on exit.\"" INT TERM EXIT

# Execute the target script.
# The wrapper script will wait for the target script to finish.
"$TARGET_SCRIPT" $TARGET_ARGS
SCRIPT_EXIT_STATUS=$? # Capture the exit status of the target script

echo "$(date): Target script finished with status $SCRIPT_EXIT_STATUS."

# The trap will be executed automatically when this script exits now.
# Exit with the same status as the target script
exit $SCRIPT_EXIT_STATUS