import argparse

parser = argparse.ArgumentParser(description="Audio Book Helper")

parser.add_argument(
    "file", help="Path to the text file to analysis.", type=str
)
args = parser.parse_args()

# print(args.file)
