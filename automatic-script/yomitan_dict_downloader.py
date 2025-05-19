import sys
import requests
import os
import glob
import shutil
import argparse
import datetime
from concurrent.futures import ThreadPoolExecutor, as_completed
from typing import List, Tuple, Dict, Optional, Callable, NamedTuple, Any, Union
from tqdm import tqdm # type: ignore

# --- Constants ---
MAX_WORKERS = 5
ACHIEVED_DIR = "achieved"
DEFAULT_TARGET_DIR = os.getcwd()
CACHE_SUBDIR_NAME = ".downloader_cache"

# --- Custom Error Types ---
class DownloaderError(Exception): pass
class HttpError(DownloaderError): pass
class ResolverError(DownloaderError): pass
class ConfigError(DownloaderError): pass

# --- Core Data Structures ---
class ResolvedDownloadTarget(NamedTuple):
    source_id: str
    base_name: str
    download_url: str
    version_str: str

    def get_target_filename(self) -> str:
        return f"{self.base_name}_{self.version_str}.zip"

ResolverFn = Callable[
    [requests.Session, Optional[str], Optional[Dict[str, Any]]],
    List[ResolvedDownloadTarget]
]

class DictionarySource(NamedTuple):
    id: str
    resolver_fn: ResolverFn
    cli_version_arg_name: Optional[str] = None
    resolver_config: Optional[Dict[str, Any]] = None

# --- HTTP Utilities (Effectful, but managed) ---
def make_http_request(
    session: requests.Session, url: str, method: str = "GET",
    timeout: int = 10, to_json: bool = True
) -> Union[Dict[Any, Any], str, requests.Response]:
    try:
        response = session.request(method, url, timeout=timeout)
        response.raise_for_status()
        if not to_json: return response
        return response.json() if response.content else {}
    except requests.exceptions.RequestException as e:
        raise HttpError(f"HTTP request failed for {method} {url}: {e}") from e
    except ValueError as e:
        raise HttpError(f"Failed to decode JSON response from {url}: {e}") from e

# --- Resolver Implementations (KTY and Jitendex - content unchanged) ---
KTY_API_REPO_URL_TEMPLATE = "https://api.github.com/repos/{repo_owner}/{repo_name}/releases"
KTY_DEFAULT_REPO_OWNER = "yomidevs"
KTY_DEFAULT_REPO_NAME = "kaikki-to-yomitan"

def _get_kty_latest_release_tag(session: requests.Session, repo_owner: str, repo_name: str) -> str:
    latest_release_url = KTY_API_REPO_URL_TEMPLATE.format(repo_owner=repo_owner, repo_name=repo_name) + "/latest"
    try:
        data = make_http_request(session, latest_release_url)
        if not isinstance(data, dict): raise ResolverError(f"Unexpected response type for latest release from {latest_release_url}")
        tag_name = data.get('tag_name')
        if not tag_name: raise ResolverError(f"Could not determine latest KTY release tag from {latest_release_url}")
        print(f"Latest KTY release version ({repo_owner}/{repo_name}): {tag_name}")
        return tag_name
    except HttpError as e: raise ResolverError(f"Failed to fetch latest KTY release tag: {e}") from e

def _get_kty_release_assets(session: requests.Session, repo_owner: str, repo_name: str, tag: str) -> List[Dict[str, Any]]:
    release_by_tag_url = KTY_API_REPO_URL_TEMPLATE.format(repo_owner=repo_owner, repo_name=repo_name) + f"/tags/{tag}"
    try:
        data = make_http_request(session, release_by_tag_url)
        if not isinstance(data, dict): raise ResolverError(f"Unexpected response type for release assets from {release_by_tag_url}")
        assets = data.get('assets', [])
        if not isinstance(assets, list): raise ResolverError(f"Assets field is not a list in response from {release_by_tag_url}")
        return assets
    except HttpError as e: raise ResolverError(f"Failed to fetch KTY release assets for tag {tag}: {e}") from e

def kty_resolver_fn(session: requests.Session, cli_version: Optional[str], config: Optional[Dict[str, Any]]) -> List[ResolvedDownloadTarget]:
    cfg = config or {}
    repo_owner = cfg.get("repo_owner", KTY_DEFAULT_REPO_OWNER)
    repo_name = cfg.get("repo_name", KTY_DEFAULT_REPO_NAME)
    asset_filter_list = cfg.get("asset_basenames_to_download")
    source_id_val = cfg.get("id", "kaikki-to-yomitan")

    version_to_fetch = cli_version
    if not version_to_fetch:
        print(f"No version provided for {source_id_val} source ({repo_name}). Fetching latest...")
        version_to_fetch = _get_kty_latest_release_tag(session, repo_owner, repo_name)
    if not version_to_fetch: raise ResolverError(f"Unable to determine a version for {source_id_val} source ({repo_name}).")

    print(f"Fetching KTY download URLs for version {version_to_fetch} from {repo_owner}/{repo_name}...")
    assets = _get_kty_release_assets(session, repo_owner, repo_name, version_to_fetch)
    targets: List[ResolvedDownloadTarget] = []
    for asset in assets:
        asset_name, download_url = asset.get("name"), asset.get("browser_download_url")
        if not (asset_name and asset_name.endswith(".zip") and download_url): continue
        base_name = asset_name[:-4]
        if asset_filter_list and base_name not in asset_filter_list: continue
        targets.append(ResolvedDownloadTarget(source_id_val, base_name, download_url, version_to_fetch))
    if not targets: print(f"Warning: No download targets resolved for {source_id_val} source ({repo_name}) with version {version_to_fetch}.")
    return targets

def jitendex_resolver_fn(session: requests.Session, cli_version: Optional[str], config: Optional[Dict[str, Any]]) -> List[ResolvedDownloadTarget]:
    cfg = config or {}
    download_url, base_name, source_id = cfg.get("download_url"), cfg.get("base_name"), cfg.get("id", "jitendex")
    if not download_url or not base_name: raise ConfigError(f"{source_id} resolver config missing 'download_url' or 'base_name'.")

    effective_version: str
    if cli_version:
        effective_version = cli_version
        print(f"Using provided version for {source_id}: {effective_version}")
    else:
        print(f"No version provided for {source_id}. Attempting to derive from Last-Modified header of {download_url}...")
        try:
            response = make_http_request(session, download_url, method="HEAD", to_json=False)
            assert isinstance(response, requests.Response)
            last_modified = response.headers.get("Last-Modified")
            if last_modified:
                dt_obj = datetime.datetime.strptime(last_modified, "%a, %d %b %Y %H:%M:%S %Z")
                effective_version = dt_obj.strftime("%Y%m%d%H%M%S")
                print(f"Derived {source_id} version from Last-Modified: {effective_version}")
            else: raise HttpError("Last-Modified header not found.")
        except (HttpError, ValueError, AttributeError) as e:
            print(f"Warning: Could not get Last-Modified for {source_id} ({e}). Using current timestamp as version.")
            effective_version = datetime.datetime.now().strftime("%Y%m%d%H%M%S")
    return [ResolvedDownloadTarget(source_id, base_name, download_url, effective_version)]

# --- File System Utilities (Effectful) ---
def ensure_directory_exists(directory: str) -> None:
    if not os.path.exists(directory):
        try:
            os.makedirs(directory, exist_ok=True)
            # print(f"Created directory: {directory}") # Less verbose
        except OSError as e: raise DownloaderError(f"Error creating directory {directory}: {e}") from e

def ensure_achieved_directory_exists(target_dir: str) -> str:
    achieved_path = os.path.join(target_dir, ACHIEVED_DIR)
    ensure_directory_exists(achieved_path)
    return achieved_path

def find_existing_versions_for_base_name(target_dir: str, base_name: str) -> List[str]:
    return glob.glob(os.path.join(target_dir, f"{base_name}_*.zip"))

def move_old_versions_to_achieved(target_dir: str, base_name: str, current_version_str: str) -> None:
    achieved_dir_path = ensure_achieved_directory_exists(target_dir)
    current_file_name_expected = f"{base_name}_{current_version_str}.zip"
    for old_file_path in find_existing_versions_for_base_name(target_dir, base_name):
        if os.path.basename(old_file_path) == current_file_name_expected: continue
        destination = os.path.join(achieved_dir_path, os.path.basename(old_file_path))
        try:
            print(f"Archiving old version '{old_file_path}' to '{destination}'")
            shutil.move(old_file_path, destination)
        except Exception as e: print(f"Warning: Failed to move '{old_file_path}' to achieved. Error: {e}")

# --- Core Download and Distribution Logic (Effectful) ---
def _perform_actual_download(download_url: str, output_full_path: str, session: requests.Session, display_filename: str) -> bool:
    temp_output_path = output_full_path + ".part"
    try:
        print(f"Downloading: {display_filename} from {download_url}")
        response = session.get(download_url, stream=True, timeout=600)
        response.raise_for_status()
        total_size = int(response.headers.get('content-length', 0))
        with open(temp_output_path, 'wb') as f, tqdm(total=total_size, unit='iB', unit_scale=True, desc=display_filename, leave=False, position=1) as pb:
            for data in response.iter_content(chunk_size=8192):
                pb.update(len(data))
                f.write(data)
        if total_size != 0 and pb.n != total_size:
            os.remove(temp_output_path)
            print(f"ERROR: Download incomplete for {display_filename}. Partial file removed.")
            return False
        shutil.move(temp_output_path, output_full_path)
        print(f"Successfully downloaded {display_filename} to {output_full_path}")
        return True
    except requests.exceptions.RequestException as e: print(f"Warning: Failed to download {display_filename}. HTTP Error: {e}")
    except IOError as e: print(f"Warning: Failed to save {display_filename}. IO Error: {e}")
    except Exception as e: print(f"Warning: Unexpected error downloading {display_filename}. Error: {e}")
    if os.path.exists(temp_output_path):
        try: os.remove(temp_output_path)
        except OSError: pass
    return False

def _download_to_cache_task(task: ResolvedDownloadTarget, cache_dir: str, session: requests.Session) -> Tuple[str, ResolvedDownloadTarget, Optional[str]]:
    target_filename = task.get_target_filename()
    cached_file_path = os.path.join(cache_dir, target_filename)
    try:
        if os.path.exists(cached_file_path) and os.path.getsize(cached_file_path) > 0:
            print(f"Cache hit: {target_filename} already in cache.")
            return "skipped_cache_hit", task, cached_file_path
    except OSError: print(f"Warning: Could not stat cached file {cached_file_path}. Attempting download.")
    
    if _perform_actual_download(task.download_url, cached_file_path, session, target_filename):
        return "success", task, cached_file_path
    return "failed", task, None

def _distribute_from_cache_task(task: ResolvedDownloadTarget, cached_file_path: str, target_dir: str) -> Tuple[str, ResolvedDownloadTarget, str]:
    target_filename = task.get_target_filename()
    destination_path = os.path.join(target_dir, target_filename)
    try:
        ensure_directory_exists(target_dir)
        move_old_versions_to_achieved(target_dir, task.base_name, task.version_str)
        if os.path.exists(destination_path):
            # Assuming if it exists with correct name after archival, it's the one we want.
            # Could add size/hash comparison with cache if needed for more robustness.
            print(f"Distribution: {target_filename} (correct version) already exists in {target_dir}. Skipping copy.")
            return "skipped_already_exists", task, target_dir
        print(f"Distributing: Copying {target_filename} from cache to {target_dir}")
        shutil.copy2(cached_file_path, destination_path)
        print(f"Distribution: Successfully copied {target_filename} to {destination_path}")
        return "success", task, target_dir
    except Exception as e:
        print(f"Warning: Failed to distribute {target_filename} to {target_dir}. Error: {e}")
        return "failed", task, target_dir

def _delete_from_cache(cached_file_path: str, display_filename: str) -> bool:
    """Attempts to delete a file from the cache. Returns True if successful or file already gone."""
    try:
        if os.path.exists(cached_file_path):
            os.remove(cached_file_path)
            print(f"Cache cleanup: Removed '{display_filename}' from cache ({cached_file_path}).")
            return True
        else:
            # print(f"Cache cleanup: File '{display_filename}' already removed or not found at {cached_file_path}.")
            return True # Considered success if already gone
    except OSError as e:
        print(f"Warning: Failed to remove '{display_filename}' from cache ({cached_file_path}). Error: {e}")
        return False

def orchestrate_downloads_and_distribution(
    all_resolved_targets: List[ResolvedDownloadTarget],
    all_target_dirs: List[str],
    session: requests.Session,
    cache_directory_path: str
) -> Dict[str, int]:
    stats = {
        "unique_downloads_successful": 0, "unique_downloads_skipped_cache_hit": 0,
        "unique_downloads_failed": 0, "distributions_successful": 0,
        "distributions_skipped_already_exists": 0, "distributions_failed": 0,
        "cache_files_deleted_after_distribution": 0, # New stat
    }
    if not all_resolved_targets: print("No download targets resolved."); return stats
    ensure_directory_exists(cache_directory_path)

    # --- Phase 1: Download unique files to cache ---
    print(f"\n--- Phase 1: Downloading {len(all_resolved_targets)} unique files to cache ({cache_directory_path}) ---")
    successfully_cached_items: List[Tuple[ResolvedDownloadTarget, str]] = []
    with ThreadPoolExecutor(max_workers=min(MAX_WORKERS, len(all_resolved_targets))) as executor:
        cache_futures = {executor.submit(_download_to_cache_task, task, cache_directory_path, session): task for task in all_resolved_targets}
        for future in tqdm(as_completed(cache_futures), total=len(all_resolved_targets), desc="Caching Downloads", unit="file", position=0, leave=True):
            status, task_obj, cached_path = future.result()
            if status == "success": stats["unique_downloads_successful"] += 1
            elif status == "skipped_cache_hit": stats["unique_downloads_skipped_cache_hit"] += 1
            else: stats["unique_downloads_failed"] += 1
            if cached_path and (status == "success" or status == "skipped_cache_hit"):
                successfully_cached_items.append((task_obj, cached_path))
    print(f"Cache phase summary: {stats['unique_downloads_successful']} downloaded, "
          f"{stats['unique_downloads_skipped_cache_hit']} cache hits, "
          f"{stats['unique_downloads_failed']} failed.")
    if not successfully_cached_items: print("No files in cache for distribution."); return stats

    # --- Phase 2: Distribute cached files to target directories ---
    print(f"\n--- Phase 2: Distributing {len(successfully_cached_items)} cached files to {len(all_target_dirs)} target director{'ies' if len(all_target_dirs) > 1 else 'y'} ---")
    distribution_jobs = [(task, cp, t_dir) for task, cp in successfully_cached_items for t_dir in all_target_dirs]
    
    cache_file_has_distribution_failures: Dict[str, bool] = {path: False for _, path in successfully_cached_items}
    cache_file_had_distribution_attempts: Dict[str, bool] = {path: False for _, path in successfully_cached_items}

    if distribution_jobs:
        with ThreadPoolExecutor(max_workers=min(MAX_WORKERS, len(distribution_jobs))) as executor:
            future_to_dist_job_details = {
                executor.submit(_distribute_from_cache_task, job_task, job_cached_path, job_target_dir):
                                (job_task, job_cached_path, job_target_dir)
                for job_task, job_cached_path, job_target_dir in distribution_jobs
            }
            for future in tqdm(as_completed(future_to_dist_job_details), total=len(distribution_jobs), desc="Distributing Files", unit="op", position=0, leave=True):
                _, original_cached_path, _ = future_to_dist_job_details[future]
                status, _, _ = future.result()
                
                cache_file_had_distribution_attempts[original_cached_path] = True
                if status == "failed":
                    cache_file_has_distribution_failures[original_cached_path] = True
                
                if status == "success": stats["distributions_successful"] += 1
                elif status == "skipped_already_exists": stats["distributions_skipped_already_exists"] += 1
                else: stats["distributions_failed"] += 1 # Counts individual copy failures
    else:
        print("No distribution jobs to process (e.g., no target directories specified).")

    # --- Phase 3: Cache Cleanup ---
    if not all_target_dirs:
        print("\nSkipping cache cleanup: No target directories were specified.")
    else:
        print(f"\n--- Phase 3: Cleaning up cache directory ({cache_directory_path}) ---")
        files_deleted_count = 0
        for task, cached_file_path in successfully_cached_items:
            target_filename = task.get_target_filename()
            attempted = cache_file_had_distribution_attempts.get(cached_file_path, False)
            had_failures = cache_file_has_distribution_failures.get(cached_file_path, True) # Default True (has failures) for safety

            if attempted and not had_failures:
                if _delete_from_cache(cached_file_path, target_filename):
                    files_deleted_count += 1
            elif attempted and had_failures:
                print(f"Retaining '{target_filename}' in cache due to one or more distribution failures.")
            elif not attempted and all_target_dirs: # Should ideally not happen if jobs created correctly
                print(f"Warning: Retaining '{target_filename}' in cache. No distribution attempts recorded for it despite target directories existing.")
            # If not attempted because all_target_dirs was empty, covered by the outer 'if not all_target_dirs'
          
          # If CACHE_DIR is empty, remove it.
        if not os.listdir(cache_directory_path):
          try:
              os.rmdir(cache_directory_path)
              print(f"Cache directory '{cache_directory_path}' is empty and has been removed.")
          except OSError as e:
              print(f"Warning: Failed to remove empty cache directory '{cache_directory_path}'. Error: {e}")
        
            
            
        stats["cache_files_deleted_after_distribution"] = files_deleted_count
        print(f"Cache cleanup summary: {files_deleted_count} file(s) removed from cache.")
    return stats

# --- Argument Parsing and Main Orchestration ---
def parse_cli_arguments(dictionary_sources: List[DictionarySource]) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Download yomitan dictionaries. Files are cached, then copied to target dirs.", formatter_class=argparse.ArgumentDefaultsHelpFormatter)
    parser.add_argument("--target_dirs", nargs="+", default=[DEFAULT_TARGET_DIR], help="Target directories for dictionaries.")
    parser.add_argument("--cache_base_dir", default=os.getcwd(), help=f"Base directory for '{CACHE_SUBDIR_NAME}'.")
    for source in dictionary_sources:
        if source.cli_version_arg_name:
            parser.add_argument(f"--{source.cli_version_arg_name}", dest=source.cli_version_arg_name, help=f"Version for {source.id}.")
    return parser.parse_args()

def resolve_all_download_targets(sources: List[DictionarySource], args: argparse.Namespace, session: requests.Session) -> List[ResolvedDownloadTarget]:
    all_targets: List[ResolvedDownloadTarget] = []
    unique_target_keys = set()
    for source in sources:
        print(f"\n--- Resolving source: {source.id} ---")
        cli_version = getattr(args, source.cli_version_arg_name, None) if source.cli_version_arg_name else None
        try:
            targets_for_source = source.resolver_fn(session, cli_version, source.resolver_config)
            newly_resolved_count = 0
            for target in targets_for_source:
                target_key = (target.source_id, target.base_name, target.version_str, target.download_url)
                if target_key not in unique_target_keys:
                    all_targets.append(target); unique_target_keys.add(target_key); newly_resolved_count += 1
                # else: print(f"Note: Duplicate resolved target skipped: {target.get_target_filename()} from {target.source_id}")
            print(f"Resolved {newly_resolved_count} new unique target(s) for {source.id}.")
        except (ResolverError, HttpError) as e: print(f"Error resolving source {source.id}: {e}. Skipped.")
        except Exception as e: print(f"Unexpected error resolving source {source.id}: {e}. Skipped."); # import traceback; traceback.print_exc()
    return all_targets

def print_summary_stats(results: Dict[str, int], num_unique_targets_resolved: int, num_target_dirs: int) -> None:
    print("\n--- Overall Summary ---")
    uds, udsch, udf = results['unique_downloads_successful'], results['unique_downloads_skipped_cache_hit'], results['unique_downloads_failed']
    ds, dsae, df = results['distributions_successful'], results['distributions_skipped_already_exists'], results['distributions_failed']
    cache_deleted = results.get('cache_files_deleted_after_distribution', 0)

    print(f"Unique dictionary files considered: {num_unique_targets_resolved}")
    print("\nCache Phase:")
    print(f"  Successfully downloaded to cache: {uds}")
    print(f"  Skipped (already in cache):      {udsch}")
    print(f"  Failed to download to cache:     {udf}")
    
    total_cached_or_found = uds + udsch
    if total_cached_or_found > 0 and num_target_dirs > 0:
        print("\nDistribution Phase:")
        print(f"  Files available from cache for distribution: {total_cached_or_found}")
        print(f"  Target directories: {num_target_dirs}")
        print(f"  Total distribution operations attempted: {total_cached_or_found * num_target_dirs}")
        print(f"    Successfully distributed:          {ds}")
        print(f"    Skipped (already in target dir): {dsae}")
        print(f"    Failed to distribute:              {df}")
    elif num_unique_targets_resolved > 0:
        print("\nDistribution Phase: Skipped (no files available from cache or no target directories).")

    if num_target_dirs > 0 and total_cached_or_found > 0:
        print("\nCache Cleanup (post-distribution):")
        print(f"  Files removed from cache: {cache_deleted} / {total_cached_or_found} (candidates)")
    
    if udf > 0 or df > 0:
        print("\nSome operations failed. Check warnings above for details. Related files may be retained in cache.")
    elif num_target_dirs > 0 and total_cached_or_found > 0 and cache_deleted < total_cached_or_found :
         print("\nNot all eligible cached files were deleted; some were retained due to distribution issues or configuration.")
    else:
        print("\nAll operations completed as expected.")
        if num_target_dirs > 0 and total_cached_or_found > 0 and cache_deleted == total_cached_or_found:
             print("All successfully cached files were also successfully distributed and removed from cache.")


# --- Define Dictionary Sources ---
KTY_STANDARD_DICTIONARIES = [
    'kty-de-de-ipa', 'kty-fr-fr-ipa', 'kty-en-en-ipa', 'kty-en-en', 'kty-en-fr', 
    'kty-en-de', 'kty-en-ja', 'kty-zh-zh', 'kty-zh-fr', 'kty-zh-de', 'kty-zh-ja', 
    'kty-fr-en', 'kty-fr-fr', 'kty-fr-de', 'kty-fr-ja', 'kty-de-ja', 'kty-de-en', 
    'kty-de-fr', 'kty-de-de', 'kty-ja-ja', 'kty-la-zh', 'kty-la-en', 'kty-la-fr', 
    'kty-la-de', 'kty-la-ja'
]
ALL_CONFIGURED_SOURCES: List[DictionarySource] = [
    DictionarySource("kaikki-to-yomitan", kty_resolver_fn, "kty_version",
        {"id": "kaikki-to-yomitan", "repo_owner": "yomidevs", "repo_name": "kaikki-to-yomitan", "asset_basenames_to_download": KTY_STANDARD_DICTIONARIES}),
    DictionarySource("jitendex", jitendex_resolver_fn, "jitendex_version",
        resolver_config = {"id": "jitendex", "download_url": "https://github.com/stephenmk/stephenmk.github.io/releases/latest/download/jitendex-yomitan.zip", "base_name": "jitendex-yomitan"}),
]

def main() -> None:
    try:
        cli_args = parse_cli_arguments(ALL_CONFIGURED_SOURCES)
        cache_dir = os.path.join(cli_args.cache_base_dir, CACHE_SUBDIR_NAME)
        print(f"Using cache directory: {os.path.abspath(cache_dir)}")

        with requests.Session() as http_session:
            http_session.headers.update({'User-Agent': 'YomitanDictionaryDownloader/1.2'})
            resolved_targets = resolve_all_download_targets(ALL_CONFIGURED_SOURCES, cli_args, http_session)
            if not resolved_targets:
                print("\nNo dictionary files to download after resolving all sources.")
                print_summary_stats({}, 0, len(cli_args.target_dirs)); return

            target_dirs_abs = [os.path.abspath(d) for d in cli_args.target_dirs]
            for t_dir in target_dirs_abs: ensure_directory_exists(t_dir)
            
            op_stats = orchestrate_downloads_and_distribution(resolved_targets, target_dirs_abs, http_session, cache_dir)
            print_summary_stats(op_stats, len(resolved_targets), len(target_dirs_abs))

    except DownloaderError as e: print(f"ERROR: {e}", file=sys.stderr); sys.exit(1)
    except KeyboardInterrupt: print("\nDownload interrupted by user.", file=sys.stderr); sys.exit(1)
    except Exception as e:
        print(f"An unexpected critical error occurred: {e}", file=sys.stderr)
        import traceback; traceback.print_exc()
        sys.exit(2)

if __name__ == "__main__":
    main()