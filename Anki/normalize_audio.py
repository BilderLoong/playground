"""
Normalize audio files in a directory using ffmpeg-normalize.
Used for optimizing audio files in Anki media collection.
"""

import os
import subprocess
import multiprocessing
from tqdm import tqdm
from glob import glob

# --- Configuration ---
INPUT_DIR = "."
OUTPUT_DIR = "normalized"
# Number of parallel processes (defaults to CPU count - 1 to keep system responsive)
NUM_WORKERS = max(1, multiprocessing.cpu_count() - 1)

# specific settings for each extension to keep size low and extension correct
FORMATS = {
    ".mp3": ["-c:a", "libmp3lame", "-b:a", "128k"],
    ".ogg": ["-c:a", "libvorbis", "-q:a", "4"],
    ".m4a": ["-c:a", "aac", "-b:a", "128k"],
    ".wav": ["-c:a", "pcm_s16le"]
}

def normalize_file(file_path):
    """
    Worker function to normalize a single file.
    """
    filename = os.path.basename(file_path)
    _, ext = os.path.splitext(filename)
    ext = ext.lower()

    if ext not in FORMATS:
        return None # Skip unsupported silently

    output_path = os.path.join(OUTPUT_DIR, filename)
    
    # --- CHECK: Skip if file already exists ---
    if os.path.exists(output_path):
        return "SKIPPED" 

    # Construct the command
    cmd = [
        "ffmpeg-normalize",
        file_path,
        "--output", output_path,
        "--extension", ext.replace(".", ""), # remove dot
        "--quiet",
        "--force"
    ]
    
    # Add codec specific arguments
    cmd.extend(FORMATS[ext])

    try:
        subprocess.run(cmd, check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    except subprocess.CalledProcessError:
        return f"Error: {filename}" # Return error to main thread if fails
        
    return None

def main():
    # 1. Setup Output
    if not os.path.exists(OUTPUT_DIR):
        os.makedirs(OUTPUT_DIR)

    # 2. Collect Files
    print(f"Scanning directory... (Using {NUM_WORKERS} parallel workers)")
    files_to_process = []
    
    for ext in FORMATS.keys():
        files_to_process.extend(glob(os.path.join(INPUT_DIR, f"*{ext}")))
        # Optional: Add uppercase check
        # files_to_process.extend(glob(os.path.join(INPUT_DIR, f"*{ext.upper()}")))

    total_files = len(files_to_process)
    print(f"Found {total_files} audio files.")

    if total_files == 0:
        return

    # 3. Process in Parallel
    skipped_count = 0
    errors = []

    with multiprocessing.Pool(NUM_WORKERS) as pool:
        results = list(tqdm(
            pool.imap_unordered(normalize_file, files_to_process),
            total=total_files,
            unit="file"
        ))

    # 4. Process Results
    for r in results:
        if r == "SKIPPED":
            skipped_count += 1
        elif r and r.startswith("Error"):
            errors.append(r)

    # 5. Final Report
    print("-" * 30)
    print(f"Processed: {total_files - skipped_count - len(errors)}")
    print(f"Skipped:   {skipped_count}")
    print(f"Errors:    {len(errors)}")
    
    if errors:
        print("\nError details:")
        for err in errors[:10]:
            print(err)
        if len(errors) > 10:
            print("...and more.")
    else:
        print("\nDone!")

if __name__ == "__main__":
    main()