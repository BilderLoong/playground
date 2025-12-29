"""
Resize and compress PNG images in a directory using mogrify and pngquant.
Used for optimizing images in Anki media collection.
"""


import os
import subprocess
import multiprocessing
from tqdm import tqdm
from glob import glob

# --- Configuration ---
INPUT_DIR = "."
# Width to resize to (only shrinks, never enlarges)
MAX_WIDTH = 1000 
# Parallel workers
NUM_WORKERS = max(1, multiprocessing.cpu_count() - 1)

def process_image(file_path):
    """
    Worker function: Resizes then Compresses a single image.
    """
    filename = os.path.basename(file_path)

    try:
        # 1. Resize (Mogrify)
        # "1000>" means resize to width 1000 only if image is larger than 1000.
        resize_cmd = ["mogrify", "-resize", f"{MAX_WIDTH}>", file_path]
        subprocess.run(resize_cmd, check=True, stderr=subprocess.DEVNULL)

        # 2. Compress (Pngquant)
        # --force overwrites the original file
        compress_cmd = [
            "pngquant", 
            "--force", 
            "--ext", ".png", 
            "--quality=10-20", 
            "--strip", 
            file_path
        ]
        subprocess.run(compress_cmd, check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        
    except subprocess.CalledProcessError:
        return f"Error: {filename}"
    except Exception as e:
        return f"Error ({e}): {filename}"

    return None

def main():
    # 1. Collect Files
    # Recursive search for .png files
    print(f"Scanning for PNGs... (Using {NUM_WORKERS} workers)")
    # glob(..., recursive=True) requires python 3.5+
    files_to_process = glob(os.path.join(INPUT_DIR, "**", "*.png"), recursive=True)
    
    total_files = len(files_to_process)
    print(f"Found {total_files} PNG files.")
    print("Before processing, total png size:", get_total_file_size(files_to_process))
    print("Before processing, average png size:", get_average_file_size(files_to_process))

    if total_files == 0:
        return

    # 2. Process in Parallel
    errors = []
    
    with multiprocessing.Pool(NUM_WORKERS) as pool:
        # imap_unordered is best for progress bars
        results = list(tqdm(
            pool.imap_unordered(process_image, files_to_process),
            total=total_files,
            unit="img"
        ))

    # 3. Report
    for r in results:
        if r:
            errors.append(r)

    if errors:
        print(f"\nCompleted with {len(errors)} errors:")
        for err in errors[:10]:
            print(err)
    else:
        print("\nSuccess! All images resized and compressed.")

    print("After processing, total png size:", get_total_file_size(files_to_process))
    print("After processing, average png size:", get_average_file_size(files_to_process))

def get_total_file_size(file_list):
    return sum(os.path.getsize(f) for f in file_list)
def get_average_file_size(file_list):
    total_size = get_total_file_size(file_list)
    return total_size / len(file_list) if file_list else 0

if __name__ == "__main__":
    main()