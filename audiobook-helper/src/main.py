import csv
from pathlib import Path
from typing import Union
from counter import get_freq_dist
from is_known_word import is_known_word

s = "Our next upswing task is to tokenize the words in the text. Word tokenization is the process of splitting the sentences in a given text into individual words. This is a requirement given that our goal is to determine the frequency of the words in the text. For this, we will use the word_tokenize() function."


def create_csv(text: str, path: Union[str, Path]) -> None:
    """
    :description: Create a csv file from a text,
                    if the given path already exist a file do nothing.
    :param text: A string of unknown words.
    :param path: path to a csv file.
    :return: no return, this function writes to a csv file instead.
    """
    with open(path, "w") as csv_file:
        writer = csv.writer(csv_file)
        writer.writerow(["Word", "Frequency", "Known"])
        for word, frequency in get_freq_dist(text).most_common():
            # TODO: Add log for progress.

            is_known = is_known_word(word)
            current_row = [word, frequency, is_known]
            print(current_row)
            writer.writerow(current_row)


def create_csv_from_file(text_file: str, csv_file: str):
    """
    :param file_name: path to a text file.
    """

    with open(text_file, "r") as f:
        text = f.read()
        create_csv(text, csv_file)
