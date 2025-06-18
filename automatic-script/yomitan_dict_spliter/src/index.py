from pathlib import Path
from typing import Iterator, List


def unpack_zip(input_zip_path: Path, output_dir: Path) -> None:
    import zipfile

    with zipfile.ZipFile(input_zip_path, "r") as zip_ref:
        zip_ref.extractall(output_dir)


def unpack_zip_with_name(input_zip_path: Path) -> None:
    unpack_zip(input_zip_path, input_zip_path.parent / input_zip_path.stem)


def pack_dir(input_dir: Path, output_zip_path: Path) -> None:
    import zipfile

    with zipfile.ZipFile(output_zip_path, "w") as zip_ref:
        for file in input_dir.glob("**/*"):
            if file.is_file():
                zip_ref.write(file, file.relative_to(input_dir))


def gather_term_bank(input_dir: Path, max_number: int) -> List[List[Path]]:
    """Gather term bank files from the input directory.
    Args:
        input_dir (Path): The directory to search for term bank files.
        max_number (int): The maximum number of element of term bank files to gather.
    """
    input_dir.glob("term_bank_*.json")

    return []


def get_json_length_sum_of_list_file(json_files: Iterator[Path]) -> int:
    return sum(map(lambda x: get_json_length(x), json_files))


def get_json_length(json_file_path: Path) -> int:
    import json

    with open(json_file_path, "r", encoding="utf-8") as file:
        data = json.load(file)
        if isinstance(data, list):
            return len(data)
        else:
            raise ValueError("JSON file does not contain a list.")


def main() -> None:
    # unpack_zip(Path(__file__).parent / "sample"/ "kty-en-en_v2025-04-08-10-54-36-00-00.zip", Path(__file__).parent / "output")
    term_bank_json = (Path(__file__).parent / "output").glob("term_bank_*.json")
    json_len = get_json_length_sum_of_list_file(term_bank_json)

    print(f"json len is {json_len}")


if __name__ == "__main__":
    main()
