import sys
import requests
import os
import glob
import shutil
import argparse
from concurrent.futures import ThreadPoolExecutor
from typing import List, Tuple, Dict, Optional
from urllib.parse import urljoin
from tqdm import tqdm

# Constants
API_URL = "https://api.github.com/repos/yomidevs/kaikki-to-yomitan/releases/latest"
BASE_FILE_NAMES: List[str] = ['kty-de-de-ipa', 'kty-fr-fr-ipa', 'kty-en-en-ipa', 'kty-en-en', 'kty-en-fr', 'kty-en-de', 'kty-en-ja', 'kty-zh-zh', 'kty-zh-fr', 'kty-zh-de', 'kty-zh-ja', 'kty-fr-en', 'kty-fr-fr', 'kty-fr-de', 'kty-fr-ja', 'kty-de-ja', 'kty-de-en', 'kty-de-fr', 'kty-de-de', 'kty-ja-ja', 'kty-la-zh', 'kty-la-en', 'kty-la-fr', 'kty-la-de', 'kty-la-ja']
MAX_WORKERS = 4 # Limit concurrent downloads to avoid overwhelming the server or system.
ACHIEVED_DIR = "achieved"

def parse_arguments() -> argparse.Namespace:
    """
    Parse and handle command-line arguments.
    
    Returns:
        argparse.Namespace: Object containing command-line arguments
    """
    parser = argparse.ArgumentParser(
        description="Download kaikki-to-yomitan dictionaries from GitHub releases",
        formatter_class=argparse.ArgumentDefaultsHelpFormatter
    )
    
    parser.add_argument(
        "version", 
        nargs="?", 
        help="Dictionary version to download (e.g., 'v2025-04-08-10-54-36-00-00'). "
             "If not provided, the latest version will be fetched automatically."
    )
    
    parser.add_argument(
        "--target_dirs", 
        nargs="+", 
        default=[os.getcwd()],
        help="Target directories to download dictionaries to. "
             "Will create directories if they don't exist. "
             "Each target directory will have its own 'achieved' subdirectory."
    )
    
    return parser.parse_args()

def get_latest_release_version() -> str:
    """
    Fetches the latest release version from the GitHub API.
    
    Returns:
        str: The latest release version.
        
    Raises:
        SystemExit: If there's an error fetching the latest version.
    """
    try:
        print("No version provided. Fetching latest release version...")
        response = requests.get(API_URL, timeout=10)
        response.raise_for_status()
        data = response.json()
        version = data.get('tag_name')
        
        if not version:
            print("Error: Could not determine latest version from GitHub API.")
            sys.exit(1)
            
        print(f"Latest release version: {version}")
        return version
    
    except requests.exceptions.RequestException as e:
        print(f"Error: Failed to fetch latest release version. {e}")
        sys.exit(1)
    except Exception as e:
        print(f"Error: An unexpected error occurred while fetching the latest version. {e}")
        sys.exit(1)

def get_version_from_args(args: argparse.Namespace) -> str:
    """
    Retrieves the version from command line arguments or fetches the latest version
    if no argument is provided.

    Args:
        args: Command line arguments parsed by argparse

    Returns:
        str: The version string.
    """
    if args.version is None:
        return get_latest_release_version()
    return args.version

def get_download_urls(version: str) -> Dict[str, str]:
    """
    Retrieves the download URLs for all available files directly from the GitHub API.
    
    Args:
        version: The release version.
        
    Returns:
        Dict[str, str]: Dictionary mapping file names to their download URLs.
        
    Raises:
        SystemExit: If there's an error fetching the download URLs.
    """
    try:
        print(f"Fetching download URLs for version {version}...")
        
        # If we're using the latest version, we might already have the API response
        if not version.startswith('v'):
            # Make a specific request for the provided version
            release_url = f"https://api.github.com/repos/yomidevs/kaikki-to-yomitan/releases/tags/{version}"
        else:
            release_url = f"https://api.github.com/repos/yomidevs/kaikki-to-yomitan/releases/tags/{version}"
            
        response = requests.get(release_url, timeout=10)
        response.raise_for_status()
        data = response.json()
        
        # Extract the download URLs from the assets
        download_urls = {}
        for asset in data.get('assets', []):
            file_name = asset.get('name')
            if file_name and file_name.endswith('.zip'):
                # Remove the .zip extension to get the base name
                base_name = file_name[:-4]
                download_url = asset.get('browser_download_url')
                if download_url:
                    download_urls[base_name] = download_url
        
        if not download_urls:
            print(f"Warning: No download URLs found for version {version}.")
            
        return download_urls
    
    except requests.exceptions.RequestException as e:
        print(f"Error: Failed to fetch download URLs. {e}")
        sys.exit(1)
    except Exception as e:
        print(f"Error: An unexpected error occurred while fetching download URLs. {e}")
        sys.exit(1)

def ensure_directory_exists(directory: str) -> None:
    """
    Ensures that a directory exists, creating it if necessary.
    
    Args:
        directory: The directory path to ensure exists
    """
    if not os.path.exists(directory):
        try:
            os.makedirs(directory)
            print(f"Created directory: {directory}")
        except Exception as e:
            print(f"Error creating directory {directory}: {e}")
            sys.exit(1)

def ensure_achieved_directory_exists(target_dir: str) -> str:
    """
    Ensures that the achieved directory exists within the target directory.
    
    Args:
        target_dir: The target directory to create the achieved directory in
        
    Returns:
        str: The path to the achieved directory
    """
    achieved_path = os.path.join(target_dir, ACHIEVED_DIR)
    ensure_directory_exists(achieved_path)
    return achieved_path

def find_existing_versions(target_dir: str, file_base_name: str) -> List[str]:
    """
    Finds all existing versions of a dictionary file in the target directory.
    
    Args:
        target_dir: The directory to search in
        file_base_name: The base name of the file (e.g., 'kty-de-en')
        
    Returns:
        List[str]: Paths to all existing versions of the file
    """
    pattern = os.path.join(target_dir, f"{file_base_name}_*.zip")
    return glob.glob(pattern)

def move_old_versions_to_achieved(target_dir: str, file_base_name: str, current_version: str) -> None:
    """
    Moves old versions of a dictionary file to the achieved directory.
    
    Args:
        target_dir: The directory containing the files
        file_base_name: The base name of the file (e.g., 'kty-de-en')
        current_version: The current version that should not be moved
    """
    achieved_dir = ensure_achieved_directory_exists(target_dir)
    current_file = f"{file_base_name}_{current_version}.zip"
    
    for old_file_path in find_existing_versions(target_dir, file_base_name):
        old_file_name = os.path.basename(old_file_path)
        # Skip the current version
        if old_file_name == current_file:
            continue
            
        # Move the old file to the achieved directory
        destination = os.path.join(achieved_dir, old_file_name)
        try:
            shutil.move(old_file_path, destination)
            print(f"Moved old version '{old_file_path}' to '{destination}'")
        except Exception as e:
            print(f"Warning: Failed to move '{old_file_path}' to achieved directory. Error: {e}")

def download_file(url: str, file_name: str, version: str, target_dir: str) -> str:
    """
    Downloads a file from the given URL and saves it to the target directory.
    If successful and older versions exist, moves them to the achieved directory.

    Args:
        url: The URL of the file to download.
        file_name: The name of the file (without extension).
        version: The release version.
        target_dir: The directory to download the file to.

    Returns:
        str: "success", "skipped", or "failed" indicating the download status.
    """
    ensure_directory_exists(target_dir)
    download_file_name = f"{file_name}_{version}.zip"
    download_path = os.path.join(target_dir, download_file_name)
    
    if os.path.exists(download_path):
        print(f"File {download_path} already exists. Skipping download.")
        return "skipped"

    try:
        print(f"Downloading {download_file_name} to {target_dir} from {url}...")
        response = requests.get(url, stream=True, timeout=30)
        response.raise_for_status()

        total_size_in_bytes = int(response.headers.get('content-length', 0))
        block_size = 1024
        progress_bar = tqdm(total=total_size_in_bytes, unit='iB', unit_scale=True, desc=download_file_name, leave=False)

        with open(download_path, 'wb') as file:
            for data in response.iter_content(block_size):
                progress_bar.update(len(data))
                file.write(data)
        progress_bar.close()

        if total_size_in_bytes != 0 and progress_bar.n != total_size_in_bytes:
            print(f"ERROR: Download incomplete for {download_path}")
            return "failed"

        print(f"Downloaded {download_path} successfully.")
        
        # Move old versions to achieved directory after successful download
        move_old_versions_to_achieved(target_dir, file_name, version)
        
        return "success"

    except requests.exceptions.RequestException as e:
        print(f"Warning: Failed to download {download_path}. Error: {e}")
        return "failed"
    except Exception as e:
        print(f"Warning: An unexpected error occurred while downloading {download_path}. Error: {e}")
        return "failed"

def download_files_concurrently(version: str, base_file_names: List[str], target_dirs: List[str]) -> Dict[str, int]:
    """
    Downloads multiple files concurrently using threads to multiple target directories.

    Args:
        version: The release version for the files.
        base_file_names: A list of base file names to download.
        target_dirs: List of target directories to download files to.

    Returns:
        Dict[str, int]: A dictionary containing the counts of "success", "failed", and "skipped" downloads.
    """
    download_results: Dict[str, int] = {"success": 0, "failed": 0, "skipped": 0}
    
    # Get download URLs from the GitHub API
    download_urls = get_download_urls(version)
    
    # Filter files based on the base_file_names list
    files_to_download = {}
    missing_files = []
    
    for base_name in base_file_names:
        if base_name in download_urls:
            files_to_download[base_name] = download_urls[base_name]
        else:
            missing_files.append(base_name)
    
    if missing_files:
        print(f"Warning: The following files were not found in the release: {', '.join(missing_files)}")
    
    # Ensure all target directories exist
    for target_dir in target_dirs:
        ensure_directory_exists(target_dir)
    
    total_downloads = len(files_to_download) * len(target_dirs)
    print(f"Starting download process for {len(files_to_download)} files to {len(target_dirs)} directories ({total_downloads} total downloads).")

    # Create a list of all download tasks (file + target directory combinations)
    download_tasks = []
    for file_name, url in files_to_download.items():
        for target_dir in target_dirs:
            download_tasks.append((url, file_name, version, target_dir))
    
    with ThreadPoolExecutor(max_workers=min(MAX_WORKERS, len(download_tasks))) as executor:
        download_futures = []
        for url, file_name, version, target_dir in download_tasks:
            future = executor.submit(download_file, url, file_name, version, target_dir)
            download_futures.append(future)

        for future in tqdm(download_futures, total=len(download_tasks), desc="Overall Progress"):
            result = future.result()
            download_results[result] += 1

    return download_results

def print_download_stats(download_results: Dict[str, int], total_downloads: int) -> None:
    """
    Prints the download statistics.

    Args:
        download_results: A dictionary containing the counts of download statuses.
        total_downloads: The total number of downloads attempted.
    """
    print("\nDownload process finished.")
    print(f"Successful downloads: {download_results['success']}/{total_downloads}")
    print(f"Skipped downloads: {download_results['skipped']}/{total_downloads}")
    if download_results['failed'] > 0:
        print(f"Failed downloads: {download_results['failed']}/{total_downloads}")

def main() -> None:
    """
    Main function to orchestrate the file download process.
    """
    args = parse_arguments()
    version = get_version_from_args(args)
    target_dirs = args.target_dirs
    
    print(f"Downloading files for version: {version}")
    print(f"Target directories: {', '.join(target_dirs)}")
    
    download_results = download_files_concurrently(version, BASE_FILE_NAMES, target_dirs)
    total_downloads = len(BASE_FILE_NAMES) * len(target_dirs)
    print_download_stats(download_results, total_downloads)

if __name__ == "__main__":
    main()
