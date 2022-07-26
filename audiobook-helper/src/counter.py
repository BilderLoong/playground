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


def _remove_noise(tokens: list):
    cleaned_tokens = []

    stop_words = stopwords.words("english")
    lemmatizer = WordNetLemmatizer()

    for token, tag in pos_tag(tokens):
        token = token.lower()
        if tag.startswith("NN"):
            pos = "n"
        elif tag.startswith("VB"):
            pos = "v"
        else:
            pos = "a"

        # Remove noise words.
        if (
            len(token) > 0
            and token not in stop_words
            and token not in string.punctuation
        ):
            cleaned_tokens.append(lemmatizer.lemmatize(token, pos))

    return cleaned_tokens
