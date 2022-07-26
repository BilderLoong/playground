from typing import cast
import nltk
import string
from nltk import FreqDist
from nltk.corpus import stopwords
from nltk.tokenize import word_tokenize
from nltk.stem.wordnet import WordNetLemmatizer
from nltk.tag import pos_tag


def get_freq_dist(text: str):
    tokens = word_tokenize(text)

    clean_tokens = _remove_noise(tokens)
    return FreqDist(clean_tokens)


def contain_digit(text: str) -> bool:
    return any(i.isdigit() for i in text)


def _remove_noise(tokens: list[str]) -> list[str]:
    cleaned_tokens: list[str] = []

    stop_words = stopwords.words("english")
    lemmatizer = WordNetLemmatizer()

    # TODO: Add log for progress.
    for token, tag in pos_tag(tokens):
        token = cast(str, token).lower()
        if tag.startswith("NN"):
            pos = "n"
        elif tag.startswith("VB"):
            pos = "v"
        else:
            pos = "a"

        # Remove noise words.
        if (
            len(token) > 2
            and token not in stop_words
            and token not in string.punctuation
            and not contain_digit(token)  # Remove token contain digit.
        ):
            cleaned_tokens.append(lemmatizer.lemmatize(token, pos))

    return cleaned_tokens
