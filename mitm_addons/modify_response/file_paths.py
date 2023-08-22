import pathlib

class File_paths:
    """Class to implement an iterator
    of powers of two"""

    def __init__(self, path: str, count=0):
        self.path = path
        self.count = count

    def __iter__(self):
        return self

    def __next__(self):
        # Path should relative to the script file.
        next_name = f"{self.path}_{self.count+1}.json"

        if pathlib.Path(next_name).exists():
            return next_name
        else:
            # Reset
            self.count = 0
            return  f"{self.path}.json"
