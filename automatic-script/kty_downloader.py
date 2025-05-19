import sys
import requests
import os
import glob
import shutil
import argparse
import datetime
from concurrent.futures import ThreadPoolExecutor
from typing import List, Tuple, Dict, Optional, Callable, NamedTuple, Any, Union
from tqdm import tqdm # type: ignore

# --- Constants ---
MAX_WORKERS = 5  # Limit concurrent downloads
ACHIEVED_DIR = "achieved"
DEFAULT_TARGET_DIR = os.getcwd()

# --- Custom Error Types ---
class DownloaderError(Exception):
    """Base class for errors in this script."""
    pass

class HttpError(DownloaderError):
    """For HTTP request failures."""
    pass

class ResolverError(DownloaderError):
    """For issues during the resolution of download targets."""
    pass

class ConfigError(DownloaderError):
    """For configuration errors."""
    pass


# --- Core Data Structures ---
class ResolvedDownloadTarget(NamedTuple):
    """
    Represents a single file to be downloaded, with all necessary info resolved.
    """
    source_id: str          # Identifier for the dictionary source (e.g., "kty", "jitendex")
    base_name: str          # Base name for the file (e.g., "kty-en-en", "jitendex-yomitan")
    download_url: str       # Direct download URL
    version_str: str        # Version string for this download (e.g., tag, date, timestamp)

    def get_target_filename(self) -> str:
        """Generates the filename for saving (e.g., 'base_name_version.zip')."""
        return f"{self.base_name}_{self.version_str}.zip"

# Type for a resolver function
ResolverFn = Callable[
    [requests.Session, Optional[str], Optional[Dict[str, Any]]],
    List[ResolvedDownloadTarget]
]

class DictionarySource(NamedTuple):
    """
    Configuration for a dictionary source.
    """
    id: str                     # Unique identifier for the source (e.g., "kaikki-to-yomitan")
    resolver_fn: ResolverFn     # Function to get download targets for this source
    cli_version_arg_name: Optional[str] = None # If source takes a version via CLI (e.g., "kty_version")
    resolver_config: Optional[Dict[str, Any]] = None # Source-specific configuration


# --- HTTP Utilities (Effectful, but managed) ---
def make_http_request(
    session: requests.Session,
    url: str,
    method: str = "GET",
    timeout: int = 10,
    to_json: bool = True
) -> Union[Dict[Any, Any], str, requests.Response]:
    """
    Makes an HTTP request and handles common errors.

    Args:
        session: The requests.Session to use.
        url: The URL to request.
        method: HTTP method (GET, POST, HEAD, etc.).
        timeout: Request timeout in seconds.
        to_json: If True, attempts to parse the response as JSON.

    Returns:
        Parsed JSON (dict) or response text (str) or the full Response object if not to_json.

    Raises:
        HttpError: If the request fails or returns a non-2xx status.
    """
    try:
        response = session.request(method, url, timeout=timeout)
        response.raise_for_status()  # Raises HTTPError for bad responses (4xx or 5xx)
        if not to_json:
            return response
        return response.json() if response.content else {}
    except requests.exceptions.RequestException as e:
        raise HttpError(f"HTTP request failed for {method} {url}: {e}") from e
    except ValueError as e: # JSONDecodeError inherits from ValueError
        raise HttpError(f"Failed to decode JSON response from {url}: {e}") from e

# --- Resolver Implementations ---

# Kaikki-to-Yomitan (KTY) Resolver
KTY_API_REPO_URL_TEMPLATE = "https://api.github.com/repos/{repo_owner}/{repo_name}/releases"
KTY_DEFAULT_REPO_OWNER = "yomidevs"
KTY_DEFAULT_REPO_NAME = "kaikki-to-yomitan"

def _get_kty_latest_release_tag(
    session: requests.Session, repo_owner: str, repo_name: str
) -> str:
    """Fetches the latest release tag for a KTY-like GitHub repository."""
    latest_release_url = KTY_API_REPO_URL_TEMPLATE.format(repo_owner=repo_owner, repo_name=repo_name) + "/latest"
    try:
        data = make_http_request(session, latest_release_url)
        if not isinstance(data, dict): # Should be a dict
            raise ResolverError(f"Unexpected response type for latest release from {latest_release_url}")
        tag_name = data.get('tag_name')
        if not tag_name:
            raise ResolverError(f"Could not determine latest KTY release tag from {latest_release_url}")
        print(f"Latest KTY release version: {tag_name}")
        return tag_name
    except HttpError as e:
        raise ResolverError(f"Failed to fetch latest KTY release tag: {e}") from e

def _get_kty_release_assets(
    session: requests.Session, repo_owner: str, repo_name: str, tag: str
) -> List[Dict[str, Any]]:
    """Fetches asset data for a specific KTY-like GitHub release tag."""
    release_by_tag_url = KTY_API_REPO_URL_TEMPLATE.format(repo_owner=repo_owner, repo_name=repo_name) + f"/tags/{tag}"
    try:
        data = make_http_request(session, release_by_tag_url)
        if not isinstance(data, dict): # Should be a dict
            raise ResolverError(f"Unexpected response type for release assets from {release_by_tag_url}")
        assets = data.get('assets', [])
        if not isinstance(assets, list):
            raise ResolverError(f"Assets field is not a list in response from {release_by_tag_url}")
        return assets
    except HttpError as e:
        raise ResolverError(f"Failed to fetch KTY release assets for tag {tag}: {e}") from e

def kty_resolver_fn(
    session: requests.Session,
    cli_version: Optional[str],
    config: Optional[Dict[str, Any]]
) -> List[ResolvedDownloadTarget]:
    """
    Resolver for Kaikki-to-Yomitan dictionaries.
    Fetches release information from GitHub.
    Config options:
        `repo_owner` (str): GitHub repository owner. Default: "yomidevs".
        `repo_name` (str): GitHub repository name. Default: "kaikki-to-yomitan".
        `asset_basenames_to_download` (List[str]): Specific base names to download (e.g., ["kty-en-en", "kty-de-de-ipa"]).
                                                 If None or empty, downloads all .zip assets.
    """
    cfg = config or {}
    repo_owner = cfg.get("repo_owner", KTY_DEFAULT_REPO_OWNER)
    repo_name = cfg.get("repo_name", KTY_DEFAULT_REPO_NAME)
    asset_filter_list = cfg.get("asset_basenames_to_download")

    version_to_fetch = cli_version
    if not version_to_fetch:
        print(f"No version provided for KTY source. Fetching latest...")
        version_to_fetch = _get_kty_latest_release_tag(session, repo_owner, repo_name)
    
    if not version_to_fetch: # Should not happen if _get_kty_latest_release_tag succeeds or cli_version is set
        raise ResolverError("Unable to determine a version for KTY source.")

    print(f"Fetching KTY download URLs for version {version_to_fetch}...")
    assets = _get_kty_release_assets(session, repo_owner, repo_name, version_to_fetch)
    
    targets: List[ResolvedDownloadTarget] = []
    for asset in assets:
        asset_name = asset.get("name")
        download_url = asset.get("browser_download_url")

        if not (asset_name and asset_name.endswith(".zip") and download_url):
            continue

        base_name = asset_name[:-4]  # Remove .zip

        if asset_filter_list and base_name not in asset_filter_list:
            continue # Skip if not in the desired list

        targets.append(ResolvedDownloadTarget(
            source_id="kaikki-to-yomitan", # Or could be made dynamic via config
            base_name=base_name,
            download_url=download_url,
            version_str=version_to_fetch
        ))
    
    if not targets:
        print(f"Warning: No download targets resolved for KTY source with version {version_to_fetch}.")
    return targets


# Jitendex Resolver

def jitendex_resolver_fn(
    session: requests.Session,
    cli_version: Optional[str],
    config: Optional[Dict[str, Any]]
) -> List[ResolvedDownloadTarget]:
    """
    Resolver for Jitendex dictionary.
    Uses a fixed URL by default, version can be specified or derived.
    Config options:
        `download_url` (str): The direct download URL. Default: JITENDEX_DEFAULT_URL.
        `base_name` (str): The base name for the dictionary. Default: JITENDEX_DEFAULT_BASE_NAME.
    """
    cfg = config or {}
    download_url = cfg.get("download_url")
    base_name = cfg.get("base_name")

    effective_version: str
    if cli_version:
        effective_version = cli_version
        print(f"Using provided version for Jitendex: {effective_version}")
    else:
        # Try to get Last-Modified header for versioning
        print(f"No version provided for Jitendex. Attempting to derive from Last-Modified header of {download_url}...")
        try:
            response = make_http_request(session, download_url, method="HEAD", to_json=False)
            assert isinstance(response, requests.Response) # for type checker
            last_modified = response.headers.get("Last-Modified")
            if last_modified:
                # Parse date and reformat to YYYYMMDDHHMMSS or similar
                dt_obj = datetime.datetime.strptime(last_modified, "%a, %d %b %Y %H:%M:%S %Z")
                effective_version = dt_obj.strftime("%Y%m%d%H%M%S")
                print(f"Derived Jitendex version from Last-Modified: {effective_version}")
            else:
                raise HttpError("Last-Modified header not found.") # Caught by except below
        except (HttpError, ValueError, AttributeError) as e: # ValueError for strptime, AttributeError for None.strftime
            print(f"Warning: Could not get Last-Modified for Jitendex ({e}). Using current timestamp as version.")
            effective_version = datetime.datetime.now().strftime("%Y%m%d%H%M%S")
            
    return [ResolvedDownloadTarget(
        source_id="jitendex", # Or dynamic via config
        base_name=base_name,
        download_url=download_url,
        version_str=effective_version
    )]


# --- File System and Download Logic (Effectful) ---
def ensure_directory_exists(directory: str) -> None:
    """Ensures a directory exists, creating it if necessary."""
    if not os.path.exists(directory):
        try:
            os.makedirs(directory, exist_ok=True)
            print(f"Created directory: {directory}")
        except OSError as e:
            raise DownloaderError(f"Error creating directory {directory}: {e}") from e

def ensure_achieved_directory_exists(target_dir: str) -> str:
    """Ensures the 'achieved' subdirectory exists within the target directory."""
    achieved_path = os.path.join(target_dir, ACHIEVED_DIR)
    ensure_directory_exists(achieved_path)
    return achieved_path

def find_existing_versions_for_base_name(target_dir: str, base_name: str) -> List[str]:
    """Finds all existing versioned files for a given base name in the target directory."""
    pattern = os.path.join(target_dir, f"{base_name}_*.zip")
    return glob.glob(pattern)

def move_old_versions_to_achieved(
    target_dir: str, base_name: str, current_version_str: str
) -> None:
    """Moves old versions of a dictionary file to the achieved directory."""
    achieved_dir_path = ensure_achieved_directory_exists(target_dir)
    current_file_name_expected = f"{base_name}_{current_version_str}.zip"
    
    for old_file_path in find_existing_versions_for_base_name(target_dir, base_name):
        old_file_name = os.path.basename(old_file_path)
        if old_file_name == current_file_name_expected:
            continue # Don't move the file we might have just downloaded or are about to download.
            
        destination = os.path.join(achieved_dir_path, old_file_name)
        try:
            shutil.move(old_file_path, destination)
            print(f"Moved old version '{old_file_path}' to '{destination}'")
        except Exception as e: # Catch broad exception for shutil.move
            print(f"Warning: Failed to move '{old_file_path}' to achieved directory. Error: {e}")

def execute_single_download_task(
    task: ResolvedDownloadTarget,
    target_dir: str,
    session: requests.Session
) -> str:
    """
    Downloads a single file as per the ResolvedDownloadTarget.
    Manages file existence checks, download with progress, and archival of old versions.

    Returns:
        str: "success", "skipped", or "failed".
    """
    ensure_directory_exists(target_dir)
    target_filename = task.get_target_filename()
    download_path = os.path.join(target_dir, target_filename)

    if os.path.exists(download_path):
        # Check file size to ensure it's not a partially downloaded file (simple check)
        # A more robust check would involve checksums if available.
        try:
            if os.path.getsize(download_path) > 0: # Basic check for non-empty file
                print(f"File {download_path} already exists and is not empty. Skipping download.")
                return "skipped"
            else:
                print(f"File {download_path} exists but is empty. Attempting re-download.")
        except OSError: # File might disappear between exists and getsize
             print(f"File {download_path} existed but could not get size. Attempting re-download.")


    try:
        print(f"Downloading {target_filename} to {target_dir} from {task.download_url}...")
        # Perform archival *before* download to free up the name if an old version is the current one
        move_old_versions_to_achieved(target_dir, task.base_name, task.version_str)

        response = session.get(task.download_url, stream=True, timeout=600) # Increased timeout for large files
        response.raise_for_status()

        total_size_in_bytes = int(response.headers.get('content-length', 0))
        block_size = 8192 # Adjusted block size
        
        # Create a temporary download path to avoid partial files if interrupted
        temp_download_path = download_path + ".part"

        with open(temp_download_path, 'wb') as file, \
             tqdm(total=total_size_in_bytes, unit='iB', unit_scale=True, desc=target_filename, leave=False) as progress_bar:
            for data in response.iter_content(block_size):
                progress_bar.update(len(data))
                file.write(data)
        
        if total_size_in_bytes != 0 and progress_bar.n != total_size_in_bytes:
            os.remove(temp_download_path) # Clean up partial file
            print(f"ERROR: Download incomplete for {target_filename} ({progress_bar.n}/{total_size_in_bytes} bytes). Partial file removed.")
            return "failed"

        shutil.move(temp_download_path, download_path) # Atomic rename if possible
        print(f"Downloaded {target_filename} successfully to {download_path}.")
        return "success"

    except requests.exceptions.RequestException as e:
        print(f"Warning: Failed to download {target_filename}. HTTP Error: {e}")
        if os.path.exists(temp_download_path): os.remove(temp_download_path)
        return "failed"
    except IOError as e: # Catches file system errors during write or move
        print(f"Warning: Failed to save {target_filename}. IO Error: {e}")
        if os.path.exists(temp_download_path): os.remove(temp_download_path)
        return "failed"
    except Exception as e:
        print(f"Warning: An unexpected error occurred while downloading {target_filename}. Error: {e}")
        if 'temp_download_path' in locals() and os.path.exists(temp_download_path): # type: ignore
             os.remove(temp_download_path) # type: ignore
        return "failed"

def orchestrate_concurrent_downloads(
    all_download_targets: List[ResolvedDownloadTarget],
    target_dirs: List[str],
    session: requests.Session
) -> Dict[str, int]:
    """
    Downloads files for all resolved targets to all specified target directories, concurrently.
    """
    download_results: Dict[str, int] = {"success": 0, "failed": 0, "skipped": 0}
    
    if not all_download_targets:
        print("No download targets were resolved. Nothing to download.")
        return download_results

    # Ensure all base target directories exist before starting downloads
    for t_dir in target_dirs:
        ensure_directory_exists(t_dir)

    # Create a list of (task, target_dir) tuples for the executor
    download_jobs: List[Tuple[ResolvedDownloadTarget, str]] = [
        (target, t_dir) for target in all_download_targets for t_dir in target_dirs
    ]
    
    if not download_jobs:
        print("No download jobs to process.")
        return download_results

    num_actual_workers = min(MAX_WORKERS, len(download_jobs))
    print(f"Starting download process for {len(all_download_targets)} unique files to {len(target_dirs)} director{'y' if len(target_dirs) == 1 else 'ies'} each.")
    print(f"Total individual download operations: {len(download_jobs)}. Using up to {num_actual_workers} worker threads.")

    with ThreadPoolExecutor(max_workers=num_actual_workers) as executor:
        # Prepare futures
        futures_map = {
            executor.submit(execute_single_download_task, job_task, job_dir, session): (job_task, job_dir)
            for job_task, job_dir in download_jobs
        }

        # Process futures as they complete
        for future in tqdm(futures_map.keys(), total=len(download_jobs), desc="Overall Progress"):
            # job_task_info, job_dir_info = futures_map[future] # For debugging if needed
            try:
                result_status = future.result()
                download_results[result_status] += 1
            except Exception as e: # Should not happen if execute_single_download_task catches its exceptions
                print(f"Error processing a download future: {e}") # Log unexpected errors from future.result()
                download_results["failed"] += 1
                
    return download_results


# --- Argument Parsing and Main Orchestration ---
def parse_cli_arguments(dictionary_sources: List[DictionarySource]) -> argparse.Namespace:
    """Parses command-line arguments, dynamically adding version options for sources."""
    parser = argparse.ArgumentParser(
        description="Download various yomitan dictionaries from their sources.",
        formatter_class=argparse.ArgumentDefaultsHelpFormatter
    )
    
    parser.add_argument(
        "--target_dirs", 
        nargs="+", 
        default=[DEFAULT_TARGET_DIR],
        help="Target directories to download dictionaries to. "
             "Will create directories if they don't exist. "
             "Each target directory will have its own 'achieved' subdirectory."
    )

    # Dynamically add version arguments for each source that specifies one
    for source_config in dictionary_sources:
        if source_config.cli_version_arg_name:
            parser.add_argument(
                f"--{source_config.cli_version_arg_name}",
                dest=source_config.cli_version_arg_name, # Ensure correct attribute name in Namespace
                help=f"Version for {source_config.id} (e.g., a release tag like 'v1.2.3', a date 'YYYY-MM-DD'). "
                     "If not provided, 'latest' logic applies for this source."
            )
            
    return parser.parse_args()

def resolve_all_download_targets(
    sources: List[DictionarySource],
    args: argparse.Namespace,
    session: requests.Session
) -> List[ResolvedDownloadTarget]:
    """Iterates through all configured sources and calls their resolvers."""
    all_targets: List[ResolvedDownloadTarget] = []
    for source in sources:
        print(f"\n--- Resolving source: {source.id} ---")
        cli_version_for_source: Optional[str] = None
        if source.cli_version_arg_name:
            cli_version_for_source = getattr(args, source.cli_version_arg_name, None)
        
        try:
            targets_for_source = source.resolver_fn(
                session,
                cli_version_for_source,
                source.resolver_config
            )
            all_targets.extend(targets_for_source)
            print(f"Resolved {len(targets_for_source)} target(s) for {source.id}.")
        except ResolverError as e:
            print(f"Error resolving source {source.id}: {e}. This source will be skipped.")
        except HttpError as e:
            print(f"HTTP Error during resolution for source {source.id}: {e}. This source will be skipped.")
        except Exception as e:
            print(f"Unexpected error resolving source {source.id}: {e}. This source will be skipped.")
            # Consider re-raising for critical errors or add a traceback print for debugging
            # import traceback
            # traceback.print_exc()

    return all_targets

def print_summary_stats(results: Dict[str, int], total_expected_downloads: int) -> None:
    """Prints the final download statistics."""
    print("\n--- Download Summary ---")
    if total_expected_downloads == 0:
        print("No download operations were attempted (possibly due to no targets resolved or no target directories).")
        return

    print(f"Total individual download operations attempted: {total_expected_downloads}")
    print(f"  Successful: {results['success']}")
    print(f"  Skipped (already exist): {results['skipped']}")
    print(f"  Failed: {results['failed']}")
    
    if results['failed'] > 0:
        print("\nSome downloads failed. Check warnings above for details.")


# --- Define Dictionary Sources ---
# These are the KTY dictionary files that were originally in BASE_FILE_NAMES
# They can be used as a filter for the KTY resolver.
KTY_STANDARD_DICTIONARIES = [
    'kty-de-de-ipa', 'kty-fr-fr-ipa', 'kty-en-en-ipa', 'kty-en-en', 'kty-en-fr', 
    'kty-en-de', 'kty-en-ja', 'kty-zh-zh', 'kty-zh-fr', 'kty-zh-de', 'kty-zh-ja', 
    'kty-fr-en', 'kty-fr-fr', 'kty-fr-de', 'kty-fr-ja', 'kty-de-ja', 'kty-de-en', 
    'kty-de-fr', 'kty-de-de', 'kty-ja-ja', 'kty-la-zh', 'kty-la-en', 'kty-la-fr', 
    'kty-la-de', 'kty-la-ja'
]

ALL_CONFIGURED_SOURCES: List[DictionarySource] = [
    DictionarySource(
        id="kaikki-to-yomitan",
        resolver_fn=kty_resolver_fn,
        cli_version_arg_name="kty_version", # e.g., --kty_version v2025-04-08...
        resolver_config={
            "asset_basenames_to_download": KTY_STANDARD_DICTIONARIES
        }
    ),
    DictionarySource(
        id="jitendex",
        resolver_fn=jitendex_resolver_fn,
        cli_version_arg_name="jitendex_version", # e.g., --jitendex_version 20230101
        resolver_config={
            "download_url": "https://github.com/stephenmk/stephenmk.github.io/releases/latest/download/jitendex-yomitan.zip", 
            "base_name": "jitendex-yomitan"
        }
    ),
    # Add more DictionarySource tuples here for other dictionaries
]


def main() -> None:
    """Main function to orchestrate the dictionary download process."""
    try:
        cli_args = parse_cli_arguments(ALL_CONFIGURED_SOURCES)
        
        # Setup a shared requests session
        # This is an effectful object but managed and passed explicitly
        with requests.Session() as http_session:
            # Add a User-Agent to be polite
            http_session.headers.update({'User-Agent': 'YomitanDictionaryDownloader/1.0'})

            # 1. Resolve all download targets from all sources
            resolved_download_targets = resolve_all_download_targets(ALL_CONFIGURED_SOURCES, cli_args, http_session)
            
            if not resolved_download_targets:
                print("\nNo dictionary files to download after resolving all sources.")
                return

            # 2. Perform downloads concurrently
            target_directories = cli_args.target_dirs
            download_stats = orchestrate_concurrent_downloads(resolved_download_targets, target_directories, http_session)

            # 3. Print summary
            total_ops = len(resolved_download_targets) * len(target_directories)
            print_summary_stats(download_stats, total_ops)

    except DownloaderError as e: # Catch custom errors
        print(f"ERROR: {e}", file=sys.stderr)
        sys.exit(1)
    except KeyboardInterrupt:
        print("\nDownload process interrupted by user.", file=sys.stderr)
        sys.exit(1)
    except Exception as e: # Catch any other unexpected errors
        print(f"An unexpected critical error occurred: {e}", file=sys.stderr)
        # For debugging, you might want to print the full traceback
        # import traceback
        # traceback.print_exc()
        sys.exit(2)

if __name__ == "__main__":
    main()