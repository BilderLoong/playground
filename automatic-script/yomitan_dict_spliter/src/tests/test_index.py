import os
import pytest
import json
from pathlib import Path
from typing import List, Dict, Any


# Helper function to encapsulate the side effect of writing a JSON file
def _write_json_file(file_path: Path, content: Any) -> None:
    """Writes content to a JSON file."""
    with open(file_path, "w") as f:
        json.dump(content, f)


@pytest.fixture
def prepared_term_bank(tmp_path: Path) -> Path:
    data_dir: Path = tmp_path / "dict"
    data_dir.mkdir()

    num_files: int = 5
    content_per_file: List[int] = list(range(1, 11))  # [1, 2, ..., 10]

    # Pure computation to generate file specifications
    file_specifications: List[Dict[str, Any]] = list(
        map(
            lambda i: {
                "path": data_dir / f"term_bank_{i + 1}.json",
                "content": content_per_file,
            },
            range(num_files),
        )
    )

    # Execute side effects (file writing)
    for spec in file_specifications:
        _write_json_file(spec["path"], spec["content"])

    return data_dir


def test_gather_term_bank(prepared_term_bank):
    from src.index import gather_term_bank

    assert gather_term_bank(Path(prepared_term_bank), 11) == [
        [Path(prepared_term_bank / f"term_bank_{i + 1}.json") for i in range(5)]
    ]
