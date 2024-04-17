import os
from pathlib import Path
import pathlib
import sys
from pytest_mock import mocker

print(sys.path)

from modify_response import (
    get_json_file_content,
    get_mock_file_content,
    get_python_file_content,
)

__dir = pathlib.Path(__file__).parent


def test_json_file_content():
    path = Path(__dir / "mock").resolve().as_posix()
    res = get_json_file_content(f"{path}.json")
    assert res == "{}"


def test_python_file_content():
    path = Path(__dir / "mock").resolve().as_posix()
    res = get_python_file_content(f"{path}.py")
    assert res == "{}"
