import sys
import argparse
from pathlib import Path
from main import create_csv_from_file

my_parser = argparse.ArgumentParser(description="Audio Book Helper")

my_parser.add_argument(
    "file", help="Path to the text file to analysis.", type=str
)

my_parser.add_argument(
    "csv",
    help="Path to store the csv file. if file already existed, do nothing.",
    type=str,
)

args = my_parser.parse_args()

if not Path(args.file).exists():
    print("the given text file doesn't exists.")
    sys.exit(1)

if Path(args.csv).exists():
    print("the given csv file already exists.")
    sys.exit(1)

create_csv_from_file(args.file, args.csv)
