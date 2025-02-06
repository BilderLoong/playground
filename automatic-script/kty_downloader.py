"""
Write a Python script to download files with the given `{version}` and a list of `{file_name}` to the current location.

## Functional requirements
- Read `{version}` from the 1st command line argument.
- from this URL: https://github.com/yomidevs/kaikki-to-yomitan/releases/download/{version}/{file_name}.zip
- `{file_name}` is one item in the `{file_names}`
- `{file_names}` is `['kty-de-de-ipa', 'kty-fr-fr-ipa', 'kty-en-en-ipa', 'kty-en-en', 'kty-en-fr', 'kty-en-de', 'kty-en-ja', 'kty-zh-zh', 'kty-zh-fr', 'kty-zh-de', 'kty-zh-ja', 'kty-fr-en', 'kty-fr-fr', 'kty-fr-de', 'kty-fr-ja', 'kty-de-ja', 'kty-de-en', 'kty-de-fr', 'kty-de-de', 'kty-ja-ja', 'kty-la-zh', 'kty-la-en', 'kty-la-fr', 'kty-la-de', 'kty-la-ja']`.
- The download file name is in the format of `{file_name}_{version}.zip`
- If there is already a file with the same name skip it.
- Show the download process for each file as while as the whole downloading process.
- Show the final download stat including the success and failure.
  - Report the `{file_name}` that fail to download.
- If one of the file downloads fails skip it and warning.
- Downloading files concurrently.

## Code standard requirements
- Clearly commented.
- Using functional programming only, no OOP stuff!
- Write maintainable code.
- Handel error, consider wrong input, etc.
- Clearly typing.
"""

import sys
import requests
import os
from concurrent.futures import ThreadPoolExecutor
from typing import List, Tuple, Dict
from urllib.parse import urljoin
from tqdm import tqdm

# Constants
BASE_URL = "https://github.com/yomidevs/kaikki-to-yomitan/releases/download/"
FILE_NAMES: List[str] = ['kty-de-de-ipa', 'kty-fr-r-ipa', 'kty-en-en-ipa', 'kty-en-en', 'kty-en-fr', 'kty-en-de', 'kty-en-ja', 'kty-zh-zh', 'kty-zh-fr', 'kty-zh-de', 'kty-zh-ja', 'kty-fr-en', 'kty-fr-fr', 'kty-fr-de', 'kty-fr-ja', 'kty-de-ja', 'kty-de-en', 'kty-de-fr', 'kty-de-de', 'kty-ja-ja', 'kty-la-zh', 'kty-la-en', 'kty-la-fr', 'kty-la-de', 'kty-la-ja']
MAX_WORKERS = 4 # Limit concurrent downloads to avoid overwhelming the server or system.

def get_version_from_cli() -> str:
    """
    Retrieves the version from the command line arguments.

    Returns:
        str: The version string.

    Raises:
        SystemExit: If the version is not provided as a command line argument.
    """
    if len(sys.argv) < 2:
        print("Error: Version must be provided as a command line argument.")
        print("Usage: python script_name.py <version>")
        sys.exit(1)
    return sys.argv[1]

def create_download_url(version: str, file_name: str) -> str:
    """
    Constructs the download URL for a given version and file name.

    Args:
        version: The release version.
        file_name: The name of the file.

    Returns:
        str: The full download URL.
    """
    return urljoin(BASE_URL, f"{version}/{file_name}.zip")

def download_file(url: str, file_name: str, version: str) -> str:
    """
    Downloads a file from the given URL and saves it to the current directory.

    Args:
        url: The URL of the file to download.
        file_name: The name of the file (without extension).
        version: The release version.

    Returns:
        str: "success", "skipped", or "failed" indicating the download status.
    """
    download_file_name = f"{file_name}_{version}.zip"
    if os.path.exists(download_file_name):
        print(f"File {download_file_name} already exists. Skipping download.")
        return "skipped"

    try:
        print(f"Downloading {download_file_name} from {url}...")
        response = requests.get(url, stream=True, timeout=30)  # Added timeout to prevent indefinite hanging
        response.raise_for_status()  # Raise HTTPError for bad responses (4xx or 5xx)

        total_size_in_bytes = int(response.headers.get('content-length', 0))
        block_size = 1024  # 1 Kibibyte
        progress_bar = tqdm(total=total_size_in_bytes, unit='iB', unit_scale=True, desc=download_file_name, leave=False) # leave=False to prevent progress bar clutter

        with open(download_file_name, 'wb') as file:
            for data in response.iter_content(block_size):
                progress_bar.update(len(data))
                file.write(data)
        progress_bar.close()

        if total_size_in_bytes != 0 and progress_bar.n != total_size_in_bytes:
            print(f"ERROR: Download incomplete for {download_file_name}")
            return "failed"

        print(f"Downloaded {download_file_name} successfully.")
        return "success"

    except requests.exceptions.RequestException as e:
        print(f"Warning: Failed to download {download_file_name}. Error: {e}")
        return "failed"
    except Exception as e:
        print(f"Warning: An unexpected error occurred while downloading {download_file_name}. Error: {e}")
        return "failed"

def download_files_concurrently(version: str, file_names: List[str]) -> Dict[str, int]:
    """
    Downloads multiple files concurrently using threads.

    Args:
        version: The release version for the files.
        file_names: A list of file names to download.

    Returns:
        Dict[str, int]: A dictionary containing the counts of "success", "failed", and "skipped" downloads.
    """
    download_results: Dict[str, int] = {"success": 0, "failed": 0, "skipped": 0}
    total_files = len(file_names)
    print(f"Starting download process for {total_files} files concurrently.")

    with ThreadPoolExecutor(max_workers=min(total_files, MAX_WORKERS)) as executor:
        download_futures = []
        for file_name in file_names:
            url = create_download_url(version, file_name)
            future = executor.submit(download_file, url, file_name, version)
            download_futures.append(future)

        for future in tqdm(download_futures, total=total_files, desc="Overall Progress"):
            result = future.result()
            download_results[result] += 1

    return download_results

def print_download_stats(download_results: Dict[str, int], total_files: int) -> None:
    """
    Prints the download statistics.

    Args:
        download_results: A dictionary containing the counts of download statuses.
        total_files: The total number of files attempted to download.
    """
    print("\nDownload process finished.")
    print(f"Successful downloads: {download_results['success']}/{total_files}")
    print(f"Skipped downloads: {download_results['skipped']}/{total_files}")
    if download_results['failed'] > 0:
        print(f"Failed downloads: {download_results['failed']}/{total_files}")

def main() -> None:
    """
    Main function to orchestrate the file download process.
    """
    version = get_version_from_cli()
    print(f"Downloading files for version: {version}")
    download_results = download_files_concurrently(version, FILE_NAMES)
    print_download_stats(download_results, len(FILE_NAMES))

if __name__ == "__main__":
    main()
